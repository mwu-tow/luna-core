module Data.Graph.Transform.Substitute where

import Prologue

{-replaceSource :: Node a -> Edge a b -> m ()-}
{-replaceSource newSrc edge = do-}
    {-oldSrcUsers <- Layer.read @Users =<< Layer.read @Source edge-}
    {-Set.delete oldSrcUsers edge-}
    {-newSrcUsers <- Layer.read @Users newSrc-}
    {-Set.insert newSrcUsers edge-}
    {-Layer.write @Source edge newSrc-}


