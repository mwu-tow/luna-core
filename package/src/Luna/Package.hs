{-# LANGUAGE OverloadedStrings #-}
module Luna.Package where

import Prologue

import qualified Data.Bimap             as Bimap
import qualified Data.Map               as Map
import qualified OCI.Data.Name          as Name
import qualified Path                   as Path
import qualified System.Directory       as Dir
import qualified System.FilePath        as FilePath
import qualified System.FilePath.Find   as Find
import qualified System.Environment     as Environment
import qualified Control.Exception.Safe as Exception

import Control.Arrow           ((&&&))
import Control.Monad.Exception (MonadException)
import Data.Bimap              (Bimap)
import Data.Map                (Map)
import Path                    (Path, Abs, Rel, File, Dir, (</>))

-- === Constants === --

packageExt :: String
packageExt = ".lunaproject"

lunaRootEnv :: String
lunaRootEnv = "LUNA_LIBS_PATH"

lunaFileExt :: String
lunaFileExt = ".luna"

localLibsPath :: Path Rel Dir
localLibsPath = $(Path.mkRelDir "local_libs")

sourceDirectory :: Path Rel Dir
sourceDirectory = $(Path.mkRelDir "src")

mainFileName :: Name.Qualified
mainFileName = "Main"

mainFuncName :: Name.Qualified
mainFuncName = "main"

-- === API === --

packageSourcesForFile :: (MonadIO m, MonadThrow m)
                      => Path Abs File
                      -> m (Maybe (Bimap (Path Abs File) Name.Qualified))
packageSourcesForFile file = do
    packageRoot <- findPackageRootForFile file
    mapM findPackageSources packageRoot

findPackageRootForFile :: (MonadIO m, MonadThrow m)
                       => Path Abs File -> m (Maybe (Path Abs Dir))
findPackageRootForFile = findPackageRoot . Path.parent

newtype PackageNotFoundException = PackageNotFoundException (Path Abs File)
    deriving Show

instance Exception PackageNotFoundException where
    displayException (PackageNotFoundException file) =
        "File \"" <> Path.toFilePath file <> "\" is not a part of any project."

packageRootForFile :: ( MonadIO m, MonadThrow m
                      , MonadException PackageNotFoundException m )
                   => Path Abs File -> m (Path Abs Dir)
packageRootForFile file = do
    maybeRoot <- findPackageRoot $ Path.parent file
    maybe (throwM (PackageNotFoundException file)) pure maybeRoot

findPackageFileForFile :: (MonadIO m, MonadThrow m)
                       => Path Abs File -> m (Maybe (Path Abs File))
findPackageFileForFile = findPackageFile . Path.parent

getRelativePathForModule :: (MonadIO m, MonadCatch m)
                         => Path Abs File
                         -> Path Abs File
                         -> m (Maybe (Path Rel File))
getRelativePathForModule packageFile =
    fmap eitherToMaybe . Exception.try . Path.stripProperPrefix
        (Path.parent packageFile)
    where
        eitherToMaybe :: Either Path.PathException (Path Rel File)
                      -> Maybe (Path Rel File)
        eitherToMaybe = either (const Nothing) Just

getLunaPackagesFromDir :: (MonadIO m, MonadThrow m)
                       => Path Abs Dir -> m [Path Abs File]
getLunaPackagesFromDir dir = do
    filesInDir <- liftIO $ Dir.listDirectory (Path.toFilePath dir)
    files      <- mapM Path.parseRelFile filesInDir
    pure . fmap (dir </>)
           $ filter (\file -> Path.fileExtension file == packageExt) files

findPackageFile :: (MonadIO m, MonadThrow m)
                => Path Abs Dir -> m (Maybe (Path Abs File))
findPackageFile dir = getLunaPackagesFromDir dir >>= \case
    [] -> let parentDir = Path.parent dir in
        if parentDir == dir then pure Nothing else findPackageFile parentDir
    [packageFile] -> pure $ Just packageFile
    _             -> pure Nothing

findPackageRoot :: (MonadIO m, MonadThrow m)
                => Path Abs Dir -> m (Maybe (Path Abs Dir))
findPackageRoot dir = getLunaPackagesFromDir dir >>= \case
    [] -> let parentDir = Path.parent dir in
        if parentDir == dir then pure Nothing else findPackageRoot parentDir
    [_] -> pure $ Just dir
    _             -> pure Nothing

getPackageName :: Path Abs Dir -> Name.Name
getPackageName =
    convert . FilePath.takeBaseName . FilePath.takeDirectory . Path.toFilePath

mkQualName :: Name.Name -> Path Rel File -> Name.Qualified
mkQualName projectName file = qualName where
    qualName        = convert $ concat nameParts
    nameParts       = [ [projectName], convert <$> path, [convert moduleName] ]
    path            = filter (/= ".") $ FilePath.splitDirectories dir
    moduleName      = FilePath.dropExtensions filename
    (dir, filename) = FilePath.splitFileName (Path.toFilePath file)

assignQualName :: Path Abs Dir -> Path Abs File
               -> (Path Abs File, Name.Qualified)
assignQualName project filePath = (filePath, qualName)
    where
        qualName         = mkQualName (getPackageName project) relFileName
        Just relFileName =
            Path.stripProperPrefix (project </> sourceDirectory) filePath

findPackageSources :: (MonadIO m, MonadThrow m)
                   => Path Abs Dir -> m (Bimap (Path Abs File) Name.Qualified)
findPackageSources project = do
    let srcDir            = Path.toFilePath $ project </> sourceDirectory
        lunaFilePredicate = Find.extension Find.~~? lunaFileExt
    lunaFiles    <- liftIO $ Find.find Find.always lunaFilePredicate srcDir
    lunaFilesAbs <- mapM Path.parseAbsFile lunaFiles
    let modules  = assignQualName project <$> lunaFilesAbs
    pure $ Bimap.fromList modules

listDependencies :: (MonadIO m, MonadThrow m)
                 => Path Abs Dir -> m [(Name.Name, FilePath.FilePath)]
listDependencies projectSrc = do
    let lunaModules     = projectSrc </> localLibsPath
        lunaModulesPath = Path.toFilePath lunaModules
    dependencies <- liftIO . Exception.tryAny
        $ Dir.listDirectory lunaModulesPath
    case dependencies of
        Left _           -> pure []
        Right directDeps -> do
            indirectDeps <- for directDeps $ \proj -> do
                path <- Path.parseRelDir proj
                listDependencies (lunaModules </> path)
            pure $ fmap (convert &&& (lunaModulesPath FilePath.</>)) directDeps
                  <> concat indirectDeps

packageImportPaths :: (MonadIO m, MonadThrow m)
                   => Path Abs Dir -> m [(Name.Name, FilePath.FilePath)]
packageImportPaths projectRoot = do
    lunaroot     <- liftIO $ Dir.canonicalizePath
        =<< Environment.getEnv lunaRootEnv
    dependencies <- listDependencies projectRoot
    let importPaths = ("Std", lunaroot <> "/Std/")
                    : (getPackageName &&& Path.toFilePath) projectRoot
                    : dependencies
    pure importPaths

fileSourcePaths :: (MonadIO m, MonadThrow m)
                => Path Abs File -> m (Map Name.Qualified FilePath.FilePath)
fileSourcePaths lunaFile = do
    lunaRoot <- liftIO $ Dir.canonicalizePath
        =<< Environment.getEnv lunaRootEnv
    let filePath    = Path.fromAbsFile lunaFile
        fileName    = FilePath.dropExtension . Path.fromRelFile
            $ Path.filename lunaFile
        fileImports = [("Std", lunaRoot <> "/Std/")]

    importPaths   <- sequence $ Path.parseAbsDir . snd <$> fileImports
    importSources <- sequence $ findPackageSources <$> importPaths

    let projSrcMap = Map.map Path.toFilePath $ foldl' Map.union Map.empty
            $ Bimap.toMapR <$> importSources
        allSrcMap  = Map.insert (convertVia @Name.Name fileName)
            (Path.toFilePath lunaFile) projSrcMap

    pure allSrcMap

isLunaPackage :: (MonadIO m, MonadThrow m)
              => Path Abs Dir -> m Bool
isLunaPackage path = isJust <$> findPackageRoot path

