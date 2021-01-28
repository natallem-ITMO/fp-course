{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task.FSAction (FsAction(..)) where

import Control.Exception ( throw, try )
import Control.Monad.Extra (ifM, unlessM, whenM)
import System.FilePath ( isAbsolute, isValid, joinPath, takeDirectory, takeExtension, takeExtension )
import System.IO.Error(isAlreadyExistsError,isDoesNotExistError,isPermissionError)
import Control.Monad.Except(liftIO, when, unless)
import Control.Monad.Trans.Reader(ReaderT,ask)
import Data.IORef(IORef, writeIORef, readIORef)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getFileSize,
    getModificationTime,
    getPermissions,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )

import Task.Types(DirEntity(..), FileEntity(..), FSError(..))
import Task.Utils(normalizeFilePathIO)


class (Monad m) => FsAction m where

-- | Change inner condition of current path
  cd :: FilePath -> m ()
  cd path = do
    finePath <- getFullOrAppend path
    ifM
      (existsDirByAddress finePath)
      (changeDirWithFullPath finePath)
      (throw $ NoSuchDirectoryError finePath)

-- | Creates directory if not exists.
-- Otherwise throw FSError AlreadyExistsDirectoryError
  mkdir :: FilePath -> m ()
  mkdir path = mkdirDoIfExists (throw $ AlreadyExistsDirectoryError path) path

-- | Prepare filepath argument to create file,
-- check if directory exists and do passed monad action if so,
--  else exists directory (and sub directory)
  mkdirDoIfExists :: m () -> FilePath -> m ()
  mkdirDoIfExists funcIfExists path = do
    dirPath <- getFullOrAppend path
    ifM
      (existsDirByAddress dirPath)
      funcIfExists
      (makeDirByAddress dirPath)

  getFullOrAppend :: FilePath -> m FilePath
  getFullOrAppend path = do
    ifM
      (isAbsolutePath path)
      (normalizeFilePath path)
      (getCurrentDir >>= flip appendAddress path >>= normalizeFilePath)

-- | touch command realization
  touch :: FilePath -> m ()
  touch path = do
    getDirectory path >>= mkdirDoIfExists (return ())
    fileName <- getFullOrAppend path
    whenM (existsFile fileName) $ throw $ AlreadyExistsFileError path
    createFileByAddress fileName
-- | cat command realization
  cat :: FilePath -> m String
  cat file = do
    filePath <- getFullOrAppend file
    unlessM (existsFile filePath) $ throw $ NoSuchFileError filePath
    readFileByAddress filePath
    
-- | rm command realization
  rm :: FilePath -> m ()
  rm file = do
    filePath <- getFullOrAppend file
    unlessM (existsFile filePath) $ throw $ NoSuchFileError filePath
    removeFileByAddress filePath

-- | rmdir command realization
  rmdir :: FilePath -> m ()
  rmdir dir = do
    dirPath <- getFullOrAppend dir
    unlessM (existsDirByAddress dirPath) $ throw $ NoSuchDirectoryError dirPath
    removeDirectoryByAddress dirPath

-- | echo command realization
  echo :: FilePath -> String -> m ()
  echo file text = do
    path <- getFullOrAppend file
    unlessM (existsFile path) $
      throw $ NoSuchFileError path
    writeToFileByAddress path text

-- | ls command realization
  ls :: FilePath -> m FileEntity
  ls file = do
    fullPath <- getFullOrAppend file
    unlessM (existsFile fullPath) $
      throw $ NoSuchFileError (fullPath++"hello")
    lsByAddress fullPath

-- | stat command realization
  stat :: FilePath -> m DirEntity
  stat file = do
    fullPath <- getFullOrAppend file
    unlessM (existsDirByAddress fullPath) $
      throw $ NoSuchFileError fullPath
    statByAddress fullPath

  getDirectory :: FilePath -> m FilePath
  getDirectory path = return $ takeDirectory path

-- | checks if it possibles to create file with such filepath
  isValidFileName :: FilePath -> m Bool -- is it possible to create file with such filepath
  isValidFileName path = return $ isValid path
  
-- | checks if given path is absolute
  isAbsolutePath :: FilePath -> m Bool
  isAbsolutePath path = return $ isAbsolute path


  grep :: FilePath -> m (Maybe String)
  dir :: m [FilePath]

-- | Return current saved path
  getCurrentDir :: m FilePath

-- | Check in world if exists directory with given full path
  existsDirByAddress :: FilePath -> m Bool

-- | Check in world if exists file with given full path
  existsFile :: FilePath -> m Bool

-- | Change inner condition to passed path
  changeDirWithFullPath :: FilePath -> m ()

-- | Shortage path and return it in absolute form
  normalizeFilePath :: FilePath -> m FilePath

-- | Create in world directory by given full path(dir mustn't exist)
  makeDirByAddress :: FilePath -> m ()

-- | Create in world file by given full path(file mustn't exits)
  createFileByAddress :: FilePath -> m ()

-- | Read text in file by given full path(file must exits)
  readFileByAddress :: FilePath -> m String

-- | Remove file by given full path(this file must exists)
  removeFileByAddress :: FilePath -> m ()

-- | Remove dir by given full path(this dir must exists)
  removeDirectoryByAddress :: FilePath -> m ()

-- | Remove dir by given full path(this dir must exists)
  writeToFileByAddress :: FilePath -> String -> m ()

-- | Return information about file (that must exists)
--   by given full path
  lsByAddress :: FilePath -> m FileEntity

-- | Return information about directory (that must exists)
--   by given full path
  statByAddress :: FilePath -> m DirEntity
-- | Return size in byte of existed file by given full path
  getSizeFileByAddress :: FilePath -> m Integer

-- | Concat two filePaths
  appendAddress :: FilePath -> FilePath -> m FilePath
  appendAddress firstFile secondFile = return $ joinPath [firstFile, secondFile]

instance FsAction (ReaderT (IORef FilePath) IO) where
  getCurrentDir = do
    ref <- ask
    do liftIO $ readIORef ref

  existsDirByAddress dir = liftIO $ doesDirectoryExist dir

  existsFile file = liftIO $ doesFileExist file

  changeDirWithFullPath path = do
    ref <- ask
    liftIO $ writeIORef ref path

  normalizeFilePath = liftIO . normalizeFilePathIO

  makeDirByAddress path = do
    (result :: Either IOError ()) <- liftIO $
      try (createDirectoryIfMissing True path)
    wrapErrorOrReturnResult False path result

  removeFileByAddress path = do
    (result :: Either IOError ()) <- liftIO $
      try (removeFile path)
    wrapErrorOrReturnResult False path result

  removeDirectoryByAddress path = do
    (result :: Either IOError ()) <- liftIO $
      try (removeDirectoryRecursive path)
    wrapErrorOrReturnResult True path result

  dir = do
    path <- getCurrentDir
    (result :: Either IOError [FilePath]) <- liftIO $ try (listDirectory path)
    wrapErrorOrReturnResult True path result

  createFileByAddress path = liftIO $ writeFile path ""
  readFileByAddress = liftIO . readFile

  isValidFileName path = return $ isValid path
  isAbsolutePath path = return $ isAbsolute path

  writeToFileByAddress path text = liftIO $ appendFile path text

  lsByAddress path = do
    permissions <- liftIO $ try (getPermissions path) >>=
      wrapErrorOrReturnResult False path
    time <- liftIO $ try (getModificationTime path) >>=
      wrapErrorOrReturnResult False path
    size <- liftIO $ try (getFileSize path) >>=
      wrapErrorOrReturnResult False path
    return
      FileEntity
        { fePath = path,
          fePermissions = show permissions,
          feSize = size,
          feType = takeExtension path,
          feTime = time
        }

  statByAddress path = do
    permissions <- liftIO $ try (getPermissions path) >>=
      wrapErrorOrReturnResult True path
    (size, filesNumber) <- sizeRec path
    return
      DirEntity
       {
           dePath = path,
           dePermissions = show permissions,
           deFilesAmount = filesNumber,
           deSize = size
         }
    where
      sizeRec :: FilePath ->ReaderT (IORef FilePath) IO (Integer, Integer)
      sizeRec path = do
        fullPath <- getFullOrAppend path
        exists <- existsDirByAddress fullPath
        unless exists $ throw $ NoSuchDirectoryError path
        calcRecursive fullPath

      calcRecursive mainPath = do
        curAppend <- getFullOrAppend mainPath
        (result :: Either IOError [FilePath]) <- liftIO $
          try (listDirectory curAppend)
        (inner :: [FilePath]) <- wrapErrorOrReturnResult
          True mainPath result
        dfs mainPath inner

      dfs _ [] = return (0,0)
      dfs currentPath (file : others) = do
        appendedAddress <- appendAddress currentPath file
        fileFull <- getFullOrAppend appendedAddress
        isDir <- existsDirByAddress fileFull
        if isDir
          then do
            result <- calcRecursive appendedAddress
            otherResult <- dfs currentPath others
            return $ sumInPairs result otherResult
          else do
            exists <- existsFile fileFull
            if exists
              then do
                curSize <- getSizeFileByAddress fileFull
                otherRes <- dfs currentPath others
                return $ sumInPairs (curSize, 1) otherRes
              else dfs currentPath others

      sumInPairs (a,b) (c,d) = (a+c, b+d)

  getSizeFileByAddress path = liftIO $ try (getFileSize path) >>=
    wrapErrorOrReturnResult False path

  grep searchFile = do
    dir <- getDirectory searchFile
    when (dir /= ".") $ throw $ IncorrectFileName $
      searchFile ++ " sfd"
    findRecursive "."
    where
      findRecursive currentAddPath= do
        curAppend <- getFullOrAppend currentAddPath
        (result :: Either IOError [FilePath]) <- liftIO $
          try (listDirectory curAppend)
        (inner :: [FilePath]) <- wrapErrorOrReturnResult True curAppend result
        dfs currentAddPath inner
      dfs _ [] = return Nothing
      dfs currentAddPath (file : others) = do
        appendedAddress <- appendAddress currentAddPath file
        fileFull <- getFullOrAppend appendedAddress
        isDir <- existsDirByAddress fileFull
        if isDir
          then do
            result <- findRecursive appendedAddress
            case result of
              Nothing -> dfs currentAddPath others
              just -> return just
          else do
            exists <- existsFile fileFull
            if exists && file == searchFile
              then return $ Just fileFull
              else dfs currentAddPath others

-- | Wrap errors if occured
wrapErrorOrReturnResult :: (Monad m) => Bool -> FilePath -> Either IOError a -> m a
wrapErrorOrReturnResult _ _ (Right res) = return res
wrapErrorOrReturnResult isDir path (Left err)
  | isPermissionError err = throw $ PermissionError path
  | isDoesNotExistError err && isDir = throw $ NoSuchDirectoryError path
  | isDoesNotExistError err = throw $ NoSuchFileError path
  | isAlreadyExistsError err && isDir = throw $ AlreadyExistsDirectoryError path
  | isAlreadyExistsError err = throw $ AlreadyExistsFileError path
  | otherwise = throw $ OtherError (show err) path
