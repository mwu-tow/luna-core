module Luna.Interpreter.Tests where

import Prologue

import qualified Control.Monad.Exception.IO      as Exception
import qualified Language.Haskell.TH             as TH
import qualified Luna.Interpreter.Tests.Internal as Internal
import qualified Path                            as Path
import qualified System.Directory                as Directory

import Control.Monad.Exception (MonadException)
import Path                    (Path, Abs, Dir, PathException)
import System.FilePath         (FilePath, (</>))



-----------------------
-- === Constants === --
-----------------------

rawDir :: FilePath
rawDir = $(do
    canPath <- TH.runIO . Directory.canonicalizePath
        $ Internal.staticFileDir </> Internal.testsRelPath
    TH.litE $ TH.StringL canPath)

-----------------
-- === API === --
-----------------

directory :: (MonadIO m, MonadException PathException m) => m (Path Abs Dir)
directory = Exception.rethrowFromIO @PathException $ Path.parseAbsDir rawDir

