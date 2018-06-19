{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Luna.Test.Spec.IRSpec where

import Prologue
import Test.Hspec.Expectations.Lifted

import qualified Control.Monad.Exception               as Exception
import qualified Data.Graph.Component.Edge.Class       as Edge
import qualified Data.Graph.Data.Component.Set         as PtrSet
import qualified Data.Graph.Data.Graph.Class           as Graph
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Graph.Fold.Class                 as Fold
import qualified Data.Graph.Fold.Partition             as Partition
import qualified Data.Graph.Fold.SubComponents         as Traversal
import qualified Data.Graph.Fold.SubTree               as Traversal
import qualified Data.Graph.Fold.SubTree               as SubTree
import qualified Data.Graph.Store                      as Store
import qualified Data.Graph.Store.Size.Discovery       as Size
import qualified Data.Mutable.Class                    as Mutable
import qualified Data.Mutable.Storable.SmallSet        as SmallSet
import qualified Data.Set                              as StdSet
import qualified Data.SmallAutoVector.Mutable.Storable as SmallVector
import qualified Data.Vector.Storable.Foreign          as Vector
import qualified Foreign.Marshal.Alloc                 as Mem
import qualified Foreign.Storable.Utils                as Storable
import qualified Luna.IR                               as IR
import qualified Luna.IR.Layer                         as Layer
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Attr                        as Attr
import qualified Luna.Pass.Basic                       as Pass
import qualified Luna.Pass.Scheduler                   as Scheduler

import Data.Graph.Data.Graph.Class           (Graph)
import Data.SmallAutoVector.Mutable.Storable (SmallVector)
import Luna.Pass                             (Pass)
import Luna.Pass.Basic                       (Compilation)
import Test.Hspec                            (Spec, describe, it)



-----------------------
-- === Test pass === --
-----------------------

-- === Definition === --

newtype IntAttr = IntAttr Int deriving (Show, Eq, Num)
type instance Attr.Type IntAttr = Attr.Atomic
instance Default IntAttr where
    def = IntAttr 0

data TestPass
type instance Pass.Spec TestPass t = TestPassSpec t
type family   TestPassSpec  t where
    TestPassSpec (Pass.In  Pass.Attrs) = '[IntAttr]
    TestPassSpec (Pass.Out Pass.Attrs) = '[IntAttr]
    TestPassSpec t                     = Pass.BasicPassSpec t


-- === API === --

type OnDemandPass stage pass m =
    ( Typeable pass
    , Pass.Compile stage pass m
    , MonadIO m
    , Exception.MonadException Scheduler.Error m
    )

runPass :: ∀ stage pass m. OnDemandPass stage pass m => Pass stage pass () -> m ()
runPass = runPasses . pure

runPasses :: ∀ stage pass m. OnDemandPass stage pass m => [Pass stage pass ()] -> m ()
runPasses passes = Scheduler.evalT $ do
    Scheduler.registerAttr     @IntAttr
    Scheduler.enableAttrByType @IntAttr
    for_ passes $ \pass -> do
        Scheduler.registerPassFromFunction__ pass
        Scheduler.runPassByType @pass

run2Passes :: ∀ stage pass m. OnDemandPass stage pass m => Pass stage pass () -> Pass stage pass () -> m ()
run2Passes p1 p2 = runPasses [p1,p2]

runPass' :: Pass Pass.Compilation TestPass () -> IO ()
runPass' p = Graph.encodeAndEval @Pass.Compilation $ runPass p

run2Passes' :: Pass Pass.Compilation TestPass () -> Pass Pass.Compilation TestPass () -> IO ()
run2Passes' p1 p2 = Graph.encodeAndEval @Pass.Compilation $ runPasses [p1,p2]



-------------------
-- === Tests === --
-------------------

nameSpec :: Spec
nameSpec = describe "names" $ do
    it "encoding" $ runPass' $ do
        v        <- IR.var "a"
        IR.Var n <- IR.model v
        n `shouldBe`    "a"
        n `shouldNotBe` "b"

irCreationSpec :: Spec
irCreationSpec = describe "ir creation" $ do
    it "single term" $ runPass' $ do
        v   <- IR.var "a"
        m   <- Layer.read @IR.Model v
        tag <- IR.showTag <$> Layer.read @IR.Model v
        m   `shouldBe` IR.UniTermVar (IR.Var "a")
        tag `shouldBe` "VAR"

    it "complex term" $ runPass' $ do
        v1           <- IR.var "a"
        v2           <- IR.var "b"
        u1           <- IR.unify v1 v2
        IR.Unify l r <- IR.model u1
        lsrc         <- Layer.read @IR.Source l
        rsrc         <- Layer.read @IR.Source r
        ltgt         <- Layer.read @IR.Target l
        rtgt         <- Layer.read @IR.Target r
        lnks         <- IR.inputs u1

        tp  <- Layer.read @IR.Type u1
        mod <- Layer.read @IR.Model u1

        lsrc `shouldBe` v1
        ltgt `shouldBe` u1
        rsrc `shouldBe` v2
        rtgt `shouldBe` u1

        u1_subComps <- StdSet.fromList . toList
                   <$> Traversal.subComponents @IR.Links u1
        let u1_subComps' = StdSet.fromList
                         [ Layout.relayout tp
                         , Layout.relayout l
                         , Layout.relayout r
                         ]

        u1_subComps `shouldBe` u1_subComps'

    it "users layer" $ runPass' $ do
        v1           <- IR.var "a"
        v2           <- IR.var "b"
        u1           <- IR.unify v1 v2
        IR.Unify l r <- IR.model u1
        v1_users     <- Mutable.toList =<< Layer.read @IR.Users v1
        v2_users     <- Mutable.toList =<< Layer.read @IR.Users v2
        v1_users `shouldBe` [Layout.relayout l]
        v2_users `shouldBe` [Layout.relayout r]

attribsSpec :: Spec
attribsSpec = describe "attributes" $ do
    it "passing between passes" $ run2Passes'
        (Attr.put $ IntAttr 9)
        (Attr.get >>= (`shouldBe` (IntAttr 9)))

irDestructSpec :: Spec
irDestructSpec = describe "ir dispose" $ do
    it "simple" $ runPass' $ do
        v <- IR.var "a"
        IR.destruct v

irDiscoverySpec :: Spec
irDiscoverySpec = describe "traversal" $ do
    it "discovery" $ runPass' $ do
        v     <- IR.var "a"
        terms <- Traversal.subTree' v

        tpl  <- Layer.read @IR.Type v
        tp   <- Layer.read @IR.Source tpl
        ttpl <- Layer.read @IR.Type tp

        let termSet  = StdSet.fromList $ toList terms
            allNodes = StdSet.fromList
                     $ [ Layout.relayout v
                       , Layout.relayout tpl
                       , Layout.relayout tp
                       , Layout.relayout ttpl
                       ]
        termSet `shouldBe` allNodes

-- partitionSpec :: Spec
-- partitionSpec = describe "Component children partition" $ do
--     it "should not be wrong" $ runPass' $ do
--         v <- IR.var "a"
--         ps <- Partition.partition v
--         st <- SubTree.subTree' v
--         1 `shouldBe` 1


testVec :: Spec
testVec = describe "test" $ it "vec" $ do
    print ""
    (print "--- vec ---" :: IO ())
    (a :: SmallVector 2 Int) <- Mutable.new
    print =<< Mutable.size a
    Mutable.pushBack a 8
    print "--"
    print =<< SmallVector.elemsPtr a
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.unsafeRead a 0
    Mutable.pushBack a 7
    print "--"
    print =<< SmallVector.elemsPtr a
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.unsafeRead a 0
    print =<< Mutable.unsafeRead a 1
    Mutable.pushBack a 6
    print "--"
    print =<< SmallVector.elemsPtr a
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.unsafeRead a 0
    print =<< Mutable.unsafeRead a 1
    print =<< Mutable.unsafeRead a 2
    Mutable.pushBack a 5
    Mutable.pushBack a 4
    Mutable.pushBack a 3
    Mutable.pushBack a 2
    Mutable.pushBack a 1
    Mutable.pushBack a 10
    print "--"
    print =<< SmallVector.elemsPtr a
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.unsafeRead a 0
    print =<< Mutable.unsafeRead a 1
    print =<< Mutable.unsafeRead a 2
    print =<< Mutable.unsafeRead a 3
    print =<< Mutable.unsafeRead a 4
    print =<< Mutable.unsafeRead a 5
    print =<< Mutable.unsafeRead a 6
    print =<< Mutable.unsafeRead a 7
    print =<< Mutable.unsafeRead a 8

    True `shouldBe` False


testSet :: Spec
testSet = describe "test" $ it "set" $ do
    print ""
    (print "--- set ---" :: IO ())
    (a :: SmallSet.SmallSet 2 Int) <- Mutable.new
    print =<< Mutable.size a
    Mutable.insert a 8
    print "--"
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print a
    Mutable.insert a 7
    print "--"
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print a
    Mutable.insert a 6
    Mutable.remove a 7
    Mutable.insert a 8
    print "--"
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print a
    Mutable.insert a 5
    Mutable.insert a 4
    Mutable.insert a 3
    Mutable.insert a 2
    Mutable.insert a 1
    Mutable.insert a 10
    print "--"
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print a

    True `shouldBe` False

test :: Spec
test = describe "test" $ it "test" $ runPass' $ do

    print "=============="
    print "=============="
    print "=============="

    v <- IR.var "a"
    v2 <- IR.var "a"
    vn <- SmallVector.fromList ["foo", "bar", "baz"]

    print "vvvvvvvvvv"
    u <- IR.update v vn v
    print "^^^^^^^^^^"

    print $ ": v  = " <> show v
    print $ ": v2 = " <> show v2
    print $ ": u  = " <> show u

    users <- Layer.read @IR.Users v
    tp    <- Layer.read @IR.Type v
    print $ "tp: " <> show tp
    print =<< PtrSet.toList users
    print =<< PtrSet.size   users
    print "***"
    -- print =<< Size.discover u
    True `shouldBe` True

spec :: Spec
spec = do

    -- testVec
    -- testSet
    test
    nameSpec
    irCreationSpec
    attribsSpec
    irDestructSpec
    irDiscoverySpec
    -- partitionSpec
