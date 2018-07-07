module Luna.Shell.Option (module Luna.Shell.Option, module X) where

import Options.Applicative as X ( execParser, info, fullDesc, progDesc, header
                                , helper )

import Prologue hiding (init)

import qualified Luna.Shell.Command   as Command
import qualified Options.Applicative  as Options

import Luna.Shell.Command   (Command)
import Options.Applicative  (Parser)



----------------------------------
-- === Command-Line Parsers === --
----------------------------------

-- === API === --

parseLunaCommand :: Parser Command
parseLunaCommand = Options.hsubparser
    (Options.command "run" (Options.info run
        (Options.progDesc "Execute a luna package, or standalone file.")))

run :: Parser Command
run = Command.Run <$> (Command.RunOpts
    <$> Options.strOption (Options.long "file"
        <> Options.metavar "FILE/FOLDER" <> Options.value ""
        <> Options.help "Execute FILE in interpreted mode.")
    <*> Options.strOption (Options.long "project"
        <> Options.metavar "FOLDER" <> Options.value ""
        <> Options.help "Execute the project in FOLDER." ))

