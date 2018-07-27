module Main where

import Prologue

import qualified Luna.Interpreter.Tests as Tests
import qualified Luna.Package           as Package
import qualified Path                   as Path
import qualified System.Directory       as Directory
import qualified System.FilePath        as FilePath



------------------
-- === Main === --
------------------

main :: IO ()
main = do
    testsDir <- Path.toFilePath <$> Tests.directory
    putStrLn $ "Executing benchmarks in " <> testsDir
    benchmarkProjects <- Directory.listDirectory testsDir
    print benchmarkProjects
    pure ()

