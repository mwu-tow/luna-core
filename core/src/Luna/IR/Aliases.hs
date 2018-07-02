{-# LANGUAGE PatternSynonyms #-}

module Luna.IR.Aliases where

import qualified Luna.IR.Term as IR

pattern Function n as b <- IR.UniTermFunction (IR.Function n as b)
pattern SectionLeft f a <- IR.UniTermSectionLeft (IR.SectionLeft f a)
pattern SectionRight f a <- IR.UniTermSectionRight (IR.SectionRight f a)
pattern AccSection n <- IR.UniTermAccSection (IR.AccSection n)
pattern Match t cls <- IR.UniTermMatch (IR.Match t cls)
pattern Unit n as b <- IR.UniTermUnit (IR.Unit n as b)
pattern Unify l r <- IR.UniTermUnify (IR.Unify l r)
pattern Lam i o <- IR.UniTermLam (IR.Lam i o)
pattern Missing <- IR.UniTermMissing (IR.Missing)
pattern App f a <- IR.UniTermApp (IR.App f a)
pattern Cons n a <- IR.UniTermCons (IR.Cons n a)
pattern Grouped g <- IR.UniTermGrouped (IR.Grouped g)
pattern Var n <- IR.UniTermVar (IR.Var n)
pattern Marked m n <- IR.UniTermMarked (IR.Marked m n)
pattern Marker  l <- IR.UniTermMarker (IR.Marker l)
pattern List  l <- IR.UniTermList (IR.List l)
pattern Tuple t <- IR.UniTermTuple (IR.Tuple t)
pattern Seq l r <- IR.UniTermSeq (IR.Seq l r)
pattern Blank <- IR.UniTermBlank (IR.Blank)
pattern Acc n e <- IR.UniTermAcc (IR.Acc n e)
pattern Documented doc e <- IR.UniTermDocumented (IR.Documented doc e)
pattern IRString s <- IR.UniTermRawString (IR.RawString s)
pattern IRNumber a b c <- IR.UniTermNumber (IR.Number a b c)
pattern Record a b c d e <- IR.UniTermRecord (IR.Record a b c d e)
pattern Metadata a <- IR.UniTermMetadata (IR.Metadata a)
pattern ImportHub a <- IR.UniTermImportHub (IR.ImportHub a)
pattern Import doc e <- IR.UniTermImp (IR.Imp doc e)
pattern ImportSrc a <- IR.UniTermImportSource (IR.ImportSource a)

