-- TODO: To be refactored
module Data.Graph.Component.Node.Utils where

import Prologue

import qualified Data.Graph.Component.Node.Class as Node
import qualified Data.Graph.Data.Layer.Class     as Layer
import qualified Data.Graph.Fold.SubComponents   as Traversal

import Data.Graph.Component.Edge.Class       (Edges)
import Data.Graph.Component.Node.Class       (Node)
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Data.Component.List        (ComponentList)

inputs ::
    ( Layer.Reader Node Model m
    , Layer.IsUnwrapped Node.Uni
    , Traversal.SubComponents Edges m (Node.Uni layout)
    , MonadIO m
    ) => Node layout -> m (ComponentList Edges)
inputs = Traversal.subComponents @Edges <=< Layer.read @Model
{-# INLINE inputs #-}
