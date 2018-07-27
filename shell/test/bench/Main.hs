module Main where

import Prologue

import qualified Luna.Interpreter.Tests as Tests
import qualified Path                   as Path
import qualified System.Directory       as Directory
import qualified System.FilePath        as FilePath



------------------
-- === Main === --
------------------

main :: IO ()
main = do
    testsDir <- Tests.directory
    putStrLn $ "Executing benchmarks in " <> Path.toFilePath testsDir
    pure ()

