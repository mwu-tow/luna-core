module Luna.Benchmark.Statistics.Internal where

import Prologue

import Data.List as List



-----------------
-- === API === --
-----------------

meanI :: Integral a => [a] -> a
meanI !xs = foldl' (+) 0 xs `div` List.genericLength xs
{-# INLINE meanI #-}

meanF :: Floating a => [a] -> a
meanF !xs = foldl' (+) 0 xs / List.genericLength xs
{-# INLINE meanF #-}

stddevI :: Integral a => [a] -> a
stddevI !xs = let ys = fromIntegral <$> xs in undefined
{-# INLINE stddevI #-}

stddevF :: Floating a => [a] -> a
stddevF !xs = undefined
{-# INLINE stddevF #-}

