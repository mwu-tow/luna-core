module Main where

import Prologue

import qualified Luna.Shell.Command                as Command
import qualified Luna.Shell.Option                 as Option

-- TODO [Ara] Resolve project paths (including stdlib, local libraries, source).
-- TODO [Ara] Execute a set of passes (static set for now).
-- TODO [Ara] Execute the results.
-- TODO [Ara] Gracefully display errors (once TC integrated).
-- TODO [Ara] Edit project module to help.

------------------
-- === Main === --
------------------

main :: IO ()
main = Command.runLuna =<< Option.execParser commandParser where
    commandParser = Option.info (Option.parseLunaCommand <**> Option.helper)
        (Option.fullDesc <> Option.progDesc
                            "The Luna Compiler command-line interface."
                         <> Option.header
                            "Visual and textual functional programming.")

