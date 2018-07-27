module Main where

import Prologue

import qualified Paths_luna_shell as Paths

-- TODO [AA] Use TH to get file location.



------------------
-- === Main === --
------------------

main :: IO ()
main = do
    fileLoc <- Paths.getDataFileName "Hello.luna"
    putStrLn fileLoc
    putStrLn "Running interpreter benchmark."
    pure ()

