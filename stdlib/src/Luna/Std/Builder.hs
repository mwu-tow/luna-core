{-# LANGUAGE OverloadedStrings #-}

module Luna.Std.Builder where

import Prologue

import qualified Data.Set   as Set
import qualified Data.Map   as Map
import qualified Luna.IR    as IR
import qualified Luna.Runtime as Luna
import qualified Luna.Pass.Sourcing.Data.Def as Def

import           Data.Set                                     (Set)
import           Data.Map                                     (Map)
import           Data.Char                                    (isUpper)
import Luna.Std.Instances ()

data LTp = LVar IR.Name | LCons IR.Name [LTp]

instance IsString LTp where
    fromString ""      = error "LTp.fromString: empty string does not represent a type"
    fromString s@(c:_) = let s' = convert s in if isUpper c then LCons s' [] else LVar s'

{-varNamesFromType :: LTp -> Set Name-}
{-varNamesFromType (LVar n)    = Set.singleton n-}
{-varNamesFromType (LCons n s) = varNamesFromTypes s-}

{-varNamesFromTypes :: [LTp] -> Set Name-}
{-varNamesFromTypes = Set.unions . fmap varNamesFromType-}

listLT :: LTp -> LTp
listLT t  = LCons "List"  [t]

maybeLT :: LTp -> LTp
maybeLT t = LCons "Maybe" [t]

eitherLT :: LTp -> LTp -> LTp
eitherLT l r = LCons "Either" [l, r]

{-mkType :: (MonadRef m, MonadPassManager m) => Map Name (Expr Draft) -> LTp -> SubPass TestPass m (Expr Draft, Expr Draft)-}
{-mkType vars ltp = do-}
    {-mv  <- var "a"-}
    {-res <- go ltp-}
    {-mon <- monadic res mv-}
    {-return (generalize mon, generalize mv)-}
  {-where-}
    {-go (LVar  n)    = return $ fromJust $ Map.lookup n vars-}
    {-go (LCons n fs) = fmap generalize $ mapM go fs >>= cons n-}

{-makeType :: [LTp] -> LTp -> Name -> IO (Assumptions, Rooted SomeExpr)-}
{-makeType args out outMonadName = do-}
    {-res <- runPM False $ do-}
        {-runRegs-}
        {-Pass.eval' @TestPass $ do-}
            {-let varNames = Set.toList $ varNamesFromTypes $ out : args-}
            {-vars <- fmap generalize <$> mapM var varNames-}
            {-let varMap = Map.fromList $ zip varNames vars-}
            {-fieldTypes <- mapM (mkType varMap) args-}
            {-(outType, outM) <- mkType varMap out-}
            {-pure       <- cons_ @Draft "Pure"-}
            {-outMonad   <- cons_ @Draft outMonadName-}
            {-let lamInPure o i = do-}
                    {-l  <- lam i o-}
                    {-lm <- monadic l pure-}
                    {-return $ generalize lm-}
            {-funType <- foldM lamInPure outType $ fst <$> reverse fieldTypes-}
            {-monads  <- scanM (fmap generalize .: unify) (unsafeGeneralize outMonad) (snd <$> fieldTypes)-}
            {-let lastMonad = head $ reverse monads-}
            {-replace lastMonad outM-}
            {-res <- compile $ generalize funType-}
            {-return (Assumptions def (tail monads) def def, res)-}
    {-case res of-}
        {-Right r -> return r-}
        {-Left e  -> error $ show e-}

{-makeTypePure :: [LTp] -> LTp -> IO (Assumptions, Rooted SomeExpr)-}
{-makeTypePure args out = makeType args out "Pure"-}

makeFunction :: (Luna.Units -> Luna.Value) -> [LTp] -> LTp -> IR.Name -> IO Def.Def
makeFunction val args out outMonad = do
    {-(assumptions, rooted) <- makeType args out outMonad-}
    return $ Def.Precompiled $ Def.PrecompiledDef val

makeFunctionPure :: (Luna.Units -> Luna.Value) -> [LTp] -> LTp -> IO Def.Def
makeFunctionPure val args out = makeFunction val args out "Pure"

makeFunctionIO :: (Luna.Units -> Luna.Value) -> [LTp] -> LTp -> IO Def.Def
makeFunctionIO val args out = makeFunction val args out "IO"

{-compileFunction :: Imports -> SubPass TestPass (PMStack IO) SomeExpr -> IO Function-}
{-compileFunction imps pass = do-}
    {-Right res <- runPM False $ do-}
        {-runRegs-}
        {-root   <- Pass.eval' pass-}
        {-trans  <- typecheck TgtNone imps [unsafeGeneralize root]-}
        {-let newRoot = fromJust $ Map.lookup (unsafeGeneralize root) trans-}
        {-val              <- Pass.eval' $ interpret imps newRoot-}
        {-Just (unifies :: Unifications)    <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @Unifications)-}
        {-Just (merges  :: MergeQueue)      <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @MergeQueue)-}
        {-Just (apps    :: SimplifierQueue) <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @SimplifierQueue)-}
        {-Just (accs    :: UnresolvedAccs)  <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @UnresolvedAccs)-}
        {-rooted <- Pass.eval' @TestPass $ do-}
            {-tp <- getLayer @Type root >>= readSource-}
            {-let whiteList = Set.unions [ Set.singleton (generalize tp)-}
                                       {-, Set.fromList (generalize <$> unwrap unifies)-}
                                       {-, Set.fromList (generalize <$> unwrap merges)-}
                                       {-, Set.fromList (generalize <$> unwrap apps)-}
                                       {-, Set.fromList (generalize <$> getAccs accs)-}
                                       {-]-}
            {-deepDeleteWithWhitelist root whiteList-}
            {-compile tp-}
        {-let value       = evalScopeT val def-}
            {-assumptions = Assumptions (unwrap unifies)-}
                                      {-(unwrap merges)-}
                                      {-(unwrap apps)-}
                                      {-(getAccs accs)-}
        {-return $ Function rooted value assumptions-}
    {-return res-}

{-preludeUnaryOp :: Name -> IO Function-}
{-preludeUnaryOp op = compileFunction def $ do-}
    {-a     <- var "a"-}
    {-acpl  <- acc a op-}
    {-l1    <- lam a acpl-}
    {-tpA   <- var "a"-}
    {-monA  <- var "monA"-}
    {-monB  <- var "monB"-}
    {-montA <- monadic tpA monA-}
    {-montB <- monadic tpA monB-}
    {-tl1   <- lam montA montB-}
    {-pure  <- cons_ @Draft "Pure"-}
    {-tl1M  <- monadic tl1 pure-}
    {-reconnectLayer' @UserType (Just tl1M) l1-}
    {-return $ generalize l1-}

{-preludeArithOp :: Name -> IO Function-}
{-preludeArithOp op = compileFunction def $ do-}
    {-a     <- var "a"-}
    {-b     <- var "b"-}
    {-acpl  <- acc a op-}
    {-apb   <- app acpl b-}
    {-l1    <- lam b apb-}
    {-l2    <- lam a l1-}
    {-tpA   <- var "a"-}
    {-monA  <- var "monA"-}
    {-monB  <- var "monB"-}
    {-monC  <- var "monC"-}
    {-montA <- monadic tpA monA-}
    {-montB <- monadic tpA monB-}
    {-montC <- monadic tpA monC-}
    {-tl1   <- lam montA montB-}
    {-pure  <- cons_ @Draft "Pure"-}
    {-tl1M  <- monadic tl1 pure-}
    {-tl2   <- lam montC tl1M-}
    {-tl2M  <- monadic tl2 pure-}
    {-reconnectLayer' @UserType (Just tl2M) l2-}
    {-return $ generalize l2-}

{-preludeCmpOp :: Imports -> Name -> IO Function-}
{-preludeCmpOp imports op = compileFunction imports $ do-}
    {-a    <- var "a"-}
    {-b    <- var "b"-}
    {-acpl <- acc a op-}
    {-apb  <- app acpl b-}
    {-l1   <- lam b apb-}
    {-l2   <- lam a l1-}
    {-tpA  <- var "a"-}
    {-bool' <- cons_ @Draft "Bool"-}
    {-monA <- var "monA"-}
    {-monB <- var "monB"-}
    {-monC <- var "monC"-}
    {-montA <- monadic tpA monA-}
    {-montB <- monadic bool' monB-}
    {-montC <- monadic tpA monC-}
    {-tl1   <- lam montA montB-}
    {-pure  <- cons_ @Draft "Pure"-}
    {-tl1M  <- monadic tl1 pure-}
    {-tl2   <- lam montC tl1M-}
    {-tl2M  <- monadic tl2 pure-}
    {-reconnectLayer' @UserType (Just tl2M) l2-}
    {-return $ generalize l2-}

int :: Integer -> Int
int = fromIntegral

integer :: Int -> Integer
integer = fromIntegral

real :: Real a => a -> Double
real = realToFrac
