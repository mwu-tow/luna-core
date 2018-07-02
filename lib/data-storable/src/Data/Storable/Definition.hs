{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Storable.Definition where

import Prologue

import qualified Language.Haskell.TH as TH

import Language.Haskell.TH         (Type (AppT))
import Language.Haskell.TH.Builder hiding (Field)


-------------------------------
-- === Struct definition === --
-------------------------------

-- | Term definition boilerplate
--
--   @
--       Term.define [d|
--           data SmallVectorA (t :: Memory.Management) (n :: Nat) (a :: Type)
--              = SmallVector
--               { length      :: Int
--               , capacity    :: Int
--               , externalMem :: Memory.UnmanagedPtr a
--               , localMem    :: Memory.Region t n a
--               }
--        |]
--   @
--
--   Generates:
--
--   @
--       -- === Definition === --
--
--       newtype SmallVectorA (t :: Memory.Management) (n :: Nat) (a :: Type)
--             = SmallVector (SmallVector_Struct t n a)
--
--       type SmallVector_Struct t n a = Struct t (SmallVector_Layout t n a)
--
--       type SmallVector_Layout t n a =
--          '[ "length"      -:: Int
--           , "capacity"    -:: Int
--           , "externalMem" -:: Memory.UnmanagedPtr a
--           , "localMem"    -:: Memory.Region t n a
--           ]
--
--
--       -- === Fields === --
--
--       length      :: Struct.Lens "length"
--       capacity    :: Struct.Lens "capacity"
--       externalMem :: Struct.Lens "externalMem"
--       localMem    :: Struct.Lens "localMem"
--       length      = Struct.autoLens ; {-# INLINE length      #-}
--       capacity    = Struct.autoLens ; {-# INLINE capacity    #-}
--       externalMem = Struct.autoLens ; {-# INLINE externalMem #-}
--       localMem    = Struct.autoLens ; {-# INLINE localMem    #-}
--   @


define :: Q [Dec] -> Q [Dec]
define declsQ = do
    decls <- declsQ
    concat <$> mapM defineSingle decls

defineSingle :: Dec -> Q [Dec]
defineSingle termDecl = pure [] -- case termDecl of
    -- TH.DataD _ dataName [] _ cons _
    --     -> concat <$> mapM (defineSingleCons dataName) cons
    -- _   -> fail "Unsupported data definition"

-- defineSingleCons :: Name -> TH.Con -> Q [Dec]
-- defineSingleCons dataName con = do
--     conName1 <- maybe (fail "All constructors have to be named") pure
--               $ fmap convert $ con ^. maybeName
--     param    <- newName "a"
--     let (needSmartCons, conNameStr) = case last conName1 of
--             Just '_' -> (False, unsafeInit conName1)
--             _        -> (True,  conName1)
--         isNewtype     = length (getBangTypes con) == 1
--         termDecl      = if isNewtype
--             then TH.NewtypeInstD [] ''Term.Constructor [cons' conName, var param] Nothing  con  []
--             else TH.DataInstD    [] ''Term.Constructor [cons' conName, var param] Nothing [con] []
--         conName       = TH.mkName conNameStr
--         tagName       = convert conNameStr
--         bangFields    = if isNewtype then id else
--                         namedFields %~ fmap (_2 .~ unpackStrictAnn)
--         mangleFields  = namedFields %~ fmap (_1 %~ mangleFieldName conNameStr)
--         rebindFields  = namedFields %~ fmap (_3 %~ expandField tagName param)
--         con'          = mangleFields
--                       . bangFields
--                       . rebindFields
--                       . (maybeName .~ Just conName)
--                       $ con
--         setDerivs     = derivClauses .~ [TH.DerivClause Nothing derivs]
--         derivs        = cons' <$> [''Show, ''Eq]
--         termDecl'     = (consList .~ [con'])
--                       . setDerivs
--                       $ termDecl

--         tagDecls      = Tag.familyInstance' ''Term.NodeTag conNameStr
--         isTermTagInst = TH.InstanceD Nothing []
--                         (TH.AppT (cons' ''Term.IsTermTag) (cons' tagName))
--                         []
--         format        = TH.mkName
--                       $ "Format" <> "." <> convert dataName
--         formatInst    = typeInstance ''Format.Of [cons' tagName] (cons' format)

--         fieldTypes    = fmap (view _3) . view namedFields $ con

--     instLens      <- Lens.declareLenses (pure [termDecl'])
--     instStorable  <- Storable.derive'    termDecl'
--     instStorable1 <- Storable1.derive'   termDecl'
--     instGtraverse <- GTraversable.derive termDecl'
--     smartCons     <- makeSmartCons tagName param fieldTypes

--     pure
--         -- === Definition ===
--          $ tagDecls
--         <> instLens
--         <> [ isTermTagInst
--            , formatInst
--            ]
--         <> instStorable
--         <> instStorable1
--         <> instGtraverse

--         -- === Smart constructors === --
--         <> (if needSmartCons then smartCons else [])


-- expandField :: Name -> Name -> TH.Type -> TH.Type
-- expandField self param field = app3 (cons' ''ExpandField) (cons' self)
--                                     (var param) field



-- -- === Helpers === --

-- unpackStrictAnn :: TH.Bang
-- unpackStrictAnn = TH.Bang TH.SourceUnpack TH.SourceStrict

-- mkTypeName :: (IsString a, Semigroup a) => a -> a
-- mkTypeName = ("Cons" <>)

-- mangleFieldName :: String -> Name -> Name
-- mangleFieldName sfx n = convert $ fixDuplicateRecordNamesGHCBug (convert n)
--                                <> "_" <> sfx



-- --------------------------------
-- -- === Smart constructors === --
-- --------------------------------

-- makeSmartCons :: Name -> Name -> [TH.Type] -> Q [TH.Dec]
-- makeSmartCons tName param fieldTypes = do
--     sigTypes <- inferSmartConsSigType tName param fieldTypes
--     let sigType         = fst sigTypes
--         genSigType      = snd sigTypes
--         smartConsName   = lowerCase tName
--         smartConsName'  = smartConsName <> "'"
--         smartConsSig    = TH.SigD smartConsName  sigType
--         smartConsGenSig = TH.SigD smartConsName' genSigType
--         fieldNum        = length fieldTypes
--     smartConsDef    <- makeSmartConsBody tName smartConsName fieldNum
--     smartConsGenDef <- makeSmartConsGenBody smartConsName fieldNum
--     pure $ smartConsSig : smartConsGenSig : (smartConsDef <> smartConsGenDef)

-- inferSmartConsSigType :: Name -> Name -> [TH.Type] -> Q (TH.Type, TH.Type)
-- inferSmartConsSigType name param fields = do
--     tvs <- mapM (const $ TH.newName "t") fields
--     mtv <- TH.newName "m"
--     let ins   = zipWith (app2 (cons' ''FieldCons) . var :: Name -> TH.Type -> TH.Type) tvs fields
--         outTp = inferSmartConsTypeOutput name $ zip tvs fields
--         out   = app (var mtv) outTp
--         inSig = arrows $ ins <> [out]
--         ctx   = [app2 (cons' ''Term.Creator) (cons' name) (var mtv)]
--         ptvs  = TH.PlainTV <$> (param : tvs <> [mtv])
--         sig   = TH.ForallT ptvs ctx inSig

--     genOutName <- newName "out"
--     let genOut   = app (var mtv) (var genOutName)
--         genInSig = arrows $ ins <> [genOut]
--         genCtx   = app2 (cons' ''Layout.Relayout) outTp (var genOutName) : ctx
--         genTvs   = TH.PlainTV genOutName : ptvs
--         genSig   = TH.ForallT genTvs genCtx genInSig

--     pure (sig, genSig)

-- arrows :: [TH.Type] -> TH.Type
-- arrows ts = foldl (flip arrow) a as where
--     arrow a b = AppT (AppT TH.ArrowT a) b
--     (a:as)    = reverse ts

-- inferSmartConsTypeOutput :: Name -> [(Name,TH.Type)] -> TH.Type
-- inferSmartConsTypeOutput tag ins = app (cons' ''Term) layout where
--     layout = foldr ($) base (uncurry apply <$> ins)
--     apply  = app3 (cons' ''AddToOutput) . var
--     base   = app2 (cons' ''Layout.Singleton) (cons' ''Model) (cons' tag)

-- inferSmartConsTypeOutputField :: Name -> [TH.Type] -> TH.Type
-- inferSmartConsTypeOutputField k vs = field where
--     tp    = app  (cons' ''Layout.MergeList) $ fromList vs
--     field = app2 (cons' ''TypeMap.AssocP) (cons' k) tp

-- makeSmartConsBody :: Name -> Name -> Int -> Q [TH.Dec]
-- makeSmartConsBody tname fname varNum = do
--     ins  <- newNames varNum
--     self <- newName "self"
--     let body      = app (var 'Term.newM) lam
--         seg t a = app2 (var '(<*>)) t
--                         (app2 (var 'consField) (var self) a)
--         lam       = TH.LamE [var self]
--                   $ foldl seg (app (var 'pure) (cons' tname)) (var <$> ins)
--         fn        = TH.FunD fname
--                   $ [TH.Clause (TH.VarP <$> ins) (TH.NormalB body) []]
--         inline    = TH.PragmaD (TH.InlineP fname TH.Inline TH.FunLike TH.AllPhases)

--     pure [fn,inline]

-- makeSmartConsGenBody :: Name -> Int -> Q [TH.Dec]
-- makeSmartConsGenBody fname varNum = do
--     ins <- newNames varNum
--     let fname' = fname <> "'"
--         body   = app2 (var 'fmap) (var 'Layout.relayout)
--                $ apps (var fname) (var <$> ins)
--         fn     = TH.FunD fname'
--                $ [TH.Clause (TH.VarP <$> ins) (TH.NormalB body) []]
--         inline = TH.PragmaD (TH.InlineP fname' TH.Inline TH.FunLike TH.AllPhases)
--     pure [fn, inline]



-- --------------------------------
-- -- === UniTerm generation === --
-- --------------------------------

-- -- | IMPORTANT: Use 'makeUniTerm' in a place in code where all possible
-- --              terms are already declared.
-- --
-- --   The 'makeUniTerm' function discovers all already declared terms and
-- --   creates an unified datatype for processing them. Its purpose is also
-- --   to enumerate the terms, so we can use the ordering when serializing them.
-- --
-- --   The generated code looks like:
-- --
-- --       -- === Definition === --
-- --
-- --       data UniTerm a
-- --           = UniTermTop     !(ConsTop     a)
-- --           | UniTermVar     !(ConsVar     a)
-- --           | UniTermMissing !(ConsMissing a)
-- --           | ...
-- --           deriving (Show, Eq)
-- --       Storable.derive     ''UniTerm
-- --       Storable1.derive    ''UniTerm
-- --       GTraversable.derive ''UniTerm
-- --
-- --
-- --       -- === Instances === --
-- --
-- --       instance Term.IsUni ConsTop     where toUni = UniTermTop
-- --       instance Term.IsUni ConsVar     where toUni = UniTermVar
-- --       instance Term.IsUni ConsMissing where toUni = UniTermMissing
-- --       instance ...
-- --
-- makeUniTerm :: Q [Dec]
-- makeUniTerm = do
--     let unpackInst = \case
--             TH.InstanceD _ _ (TH.AppT _ (TH.ConT n)) _ -> n
--             _ -> error "impossible"
--     termNames <- unpackInst <<$>> TH.reifyInstances ''Term.IsTermTag ["x"]
--     let dataName      = "UniTerm"
--         mkCons n      = TH.NormalC consName [(unpackStrictAnn, TH.AppT (childName) "a")] where
--             consName  = dataName <> n
--             childName = app (cons' ''Term.Constructor) (cons' n)
--         derivs        = [TH.DerivClause Nothing $ cons' <$> [''Show, ''Eq]]
--         dataDecl      = TH.DataD [] dataName ["a"] Nothing (mkCons <$> termNames)
--                         derivs
--         isUniInst n   = TH.InstanceD Nothing []
--                         (TH.AppT (cons' ''Term.IsUni) (app (cons' ''Term.Constructor) (cons' n)))
--                         [TH.ValD "toUni" (TH.NormalB . cons' $ mkUniTermName n) []]
--         isUniInsts   = isUniInst <$> termNames
--     instStorable  <- Storable.derive'    dataDecl
--     instStorable1 <- Storable1.derive'   dataDecl
--     instGtraverse <- GTraversable.derive dataDecl

--     pure $ [ dataDecl ]
--         <> instStorable
--         <> instStorable1
--         <> instGtraverse
--         <> isUniInsts

-- mkUniTermName :: (IsString a, Semigroup a) => a -> a
-- mkUniTermName = ("UniTerm" <>)
