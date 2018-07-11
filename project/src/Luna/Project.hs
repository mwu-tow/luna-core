{-# LANGUAGE OverloadedStrings #-}
module Luna.Project where

import Prologue

import qualified Data.Bimap           as Bimap
import qualified Data.Map             as Map
import qualified OCI.Data.Name        as Name
import qualified Path                 as Path
import qualified System.Directory     as Dir
import qualified System.FilePath      as FilePath
import qualified System.FilePath.Find as Find

import Control.Arrow           ((&&&))
import Control.Exception.Safe  (try, tryAny)
import Data.Bimap              (Bimap)
import Data.Map                (Map)
import OCI.Data.Name           (Name)
import OCI.Data.Name.Qualified (Qualified)
import Path                    (Path, Abs, Rel, File, Dir, (</>))
import System.Environment      (getEnv)

-- === Constants === --

projectExt :: String
projectExt = ".lunaproject"

lunaRootEnv :: String
lunaRootEnv = "LUNA_LIBS_PATH"

lunaFileExt :: String
lunaFileExt = ".luna"

localLibsPath :: Path Rel Dir
localLibsPath = $(Path.mkRelDir "local_libs")

sourceDirectory :: Path Rel Dir
sourceDirectory = $(Path.mkRelDir "src")

mainFileName :: Qualified
mainFileName = "Main"

mainFuncName :: Qualified
mainFuncName = "main"

-- === API === --

projectSourcesForFile :: (MonadIO m, MonadThrow m)
                      => Path Abs File
                      -> m (Maybe (Bimap (Path Abs File) Name.Qualified))
projectSourcesForFile file = do
    projectRoot <- findProjectRootForFile file
    mapM findProjectSources projectRoot

findProjectRootForFile :: (MonadIO m, MonadThrow m)
                       => Path Abs File -> m (Maybe (Path Abs Dir))
findProjectRootForFile = findProjectRoot . Path.parent

newtype ProjectNotFoundException = ProjectNotFoundException (Path Abs File)
    deriving Show

instance Exception ProjectNotFoundException where
    displayException (ProjectNotFoundException file) =
        "File \"" <> Path.toFilePath file <> "\" is not a part of any project."

projectRootForFile :: (MonadIO m, MonadThrow m)
                   => Path Abs File -> m (Path Abs Dir)
projectRootForFile file = do
    maybeRoot <- findProjectRoot $ Path.parent file
    maybe (throwM (ProjectNotFoundException file)) pure maybeRoot

findProjectFileForFile :: (MonadIO m, MonadThrow m)
                       => Path Abs File -> m (Maybe (Path Abs File))
findProjectFileForFile = findProjectFile . Path.parent

getRelativePathForModule :: (MonadIO m, MonadCatch m)
                         => Path Abs File
                         -> Path Abs File
                         -> m (Maybe (Path Rel File))
getRelativePathForModule projectFile =
    fmap eitherToMaybe . try . Path.stripProperPrefix (Path.parent projectFile)
    where
        eitherToMaybe :: Either Path.PathException (Path Rel File)
                      -> Maybe (Path Rel File)
        eitherToMaybe = either (const Nothing) Just

getLunaProjectsFromDir :: (MonadIO m, MonadThrow m)
                       => Path Abs Dir -> m [Path Abs File]
getLunaProjectsFromDir dir = do
    filesInDir <- liftIO $ Dir.listDirectory (Path.toFilePath dir)
    files      <- mapM Path.parseRelFile filesInDir
    pure . fmap (dir </>)
           $ filter (\file -> Path.fileExtension file == projectExt) files

findProjectFile :: (MonadIO m, MonadThrow m)
                => Path Abs Dir -> m (Maybe (Path Abs File))
findProjectFile dir = getLunaProjectsFromDir dir >>= \case
    [] -> let parentDir = Path.parent dir in
        if parentDir == dir then pure Nothing else findProjectFile parentDir
    [projectFile] -> pure $ Just projectFile
    _             -> pure Nothing

findProjectRoot :: (MonadIO m, MonadThrow m)
                => Path Abs Dir -> m (Maybe (Path Abs Dir))
findProjectRoot dir = getLunaProjectsFromDir dir >>= \case
    [] -> let parentDir = Path.parent dir in
        if parentDir == dir then pure Nothing else findProjectRoot parentDir
    [_] -> pure $ Just dir
    _             -> pure Nothing

getProjectName :: Path Abs Dir -> Name
getProjectName =
    convert . FilePath.takeBaseName . FilePath.takeDirectory . Path.toFilePath

mkQualName :: Name -> Path Rel File -> Name.Qualified
mkQualName projectName file = qualName where
    qualName        = convert $ concat [ [projectName]
                                         , convert <$> path
                                         , [convert moduleName]
                                         ]
    path            = filter (/= ".") $ FilePath.splitDirectories dir
    moduleName      = FilePath.dropExtensions filename
    (dir, filename) = FilePath.splitFileName (Path.toFilePath file)

assignQualName :: Path Abs Dir -> Path Abs File
               -> (Path Abs File, Name.Qualified)
assignQualName project filePath = (filePath, qualName)
    where
        qualName         = mkQualName (getProjectName project) relFileName
        Just relFileName =
            Path.stripProperPrefix (project </> sourceDirectory) filePath

findProjectSources :: (MonadIO m, MonadThrow m)
                   => Path Abs Dir -> m (Bimap (Path Abs File) Name.Qualified)
findProjectSources project = do
    let srcDir            = Path.toFilePath $ project </> sourceDirectory
        lunaFilePredicate = Find.extension Find.~~? lunaFileExt
    lunaFiles    <- liftIO $ Find.find Find.always lunaFilePredicate srcDir
    lunaFilesAbs <- mapM Path.parseAbsFile lunaFiles
    let modules  = assignQualName project <$> lunaFilesAbs
    pure $ Bimap.fromList modules

listDependencies :: (MonadIO m, MonadThrow m)
                 => Path Abs Dir -> m [(Name, FilePath.FilePath)]
listDependencies projectSrc = do
    let lunaModules     = projectSrc </> localLibsPath
        lunaModulesPath = Path.toFilePath lunaModules
    dependencies <- liftIO . tryAny $ Dir.listDirectory lunaModulesPath
    case dependencies of
        Left _           -> pure []
        Right directDeps -> do
            indirectDeps <- for directDeps $ \proj -> do
                path <- Path.parseRelDir proj
                listDependencies (lunaModules </> path)
            pure $ fmap (convert &&& (lunaModulesPath FilePath.</>)) directDeps
                  <> concat indirectDeps

projectImportPaths :: (MonadIO m, MonadThrow m)
                   => Path Abs Dir -> m [(Name, FilePath.FilePath)]
projectImportPaths projectRoot = do
    lunaroot     <- liftIO $ Dir.canonicalizePath =<< getEnv lunaRootEnv
    dependencies <- listDependencies projectRoot
    let importPaths = ("Std", lunaroot <> "/Std/")
                    : (getProjectName &&& Path.toFilePath) projectRoot
                    : dependencies
    pure importPaths

fileSourcePaths :: (MonadIO m, MonadThrow m)
                => Path Abs File -> m (Map Name.Qualified FilePath.FilePath)
fileSourcePaths lunaFile = do
    lunaRoot <- liftIO $ Dir.canonicalizePath =<< getEnv lunaRootEnv
    let filePath    = Path.fromAbsFile lunaFile
        fileName    = FilePath.dropExtension . Path.fromRelFile
            $ Path.filename lunaFile
        fileImports = [("Std", lunaRoot <> "/Std/")]

    importPaths   <- sequence $ Path.parseAbsDir . snd <$> fileImports
    importSources <- sequence $ findProjectSources <$> importPaths

    let projSrcMap = Map.map Path.toFilePath $ foldl' Map.union Map.empty
            $ Bimap.toMapR <$> importSources
        allSrcMap  = Map.insert (convertVia @Name fileName)
            (Path.toFilePath lunaFile) projSrcMap

    pure allSrcMap

isLunaProject :: (MonadIO m, MonadThrow m)
              => Path Abs Dir -> m Bool
isLunaProject path = isJust <$> findProjectRoot path

