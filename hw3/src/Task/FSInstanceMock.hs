{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Task.FSInstanceMock where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State.Lazy (MonadState, State, get, put, runState, when)
import Data.List (elemIndex)
import Data.Time.Clock (UTCTime (..))
import System.Directory.Internal (simplifyWindows)
import System.FilePath (splitPath, takeFileName)
import Task.FSAction (FsAction (..))
import Task.Types (FSError (..))

-- | Representation of Mock FS 
--   Hierarchy of dirs, that contains
--  files and other dirs
data MockFSTree = MockDir
  { dirName :: String,
    subDirs :: [MockFSTree],
    innerFiles :: [MockFile],
    mdPermissions :: String
  }
  deriving (Show, Eq)
  
-- | Representation of File in Mock FS
data MockFile = MockFile
  { name :: String,
    extension :: String,
    text :: String,
    mfPermissions :: String,
    size :: Integer,
    time :: UTCTime
  }
  deriving (Show, Eq)

-- | Mutable State of FS 
data FsState = FsState {currentPath :: FilePath, fs :: MockFSTree}
  deriving (Show, Eq)

-- | Transformers for Mock FS
newtype FsMock a = FsMock {pureFSInternals :: ExceptT FSError (State FsState) a}
  deriving (Functor, Applicative, Monad, MonadState FsState, MonadError FSError)

runMock :: FsState -> FsMock a -> (Either FSError a, FsState)
runMock state monad = runState (runExceptT $ pureFSInternals monad) state

{-Partial realisation of Mock FS-}
instance FsAction FsMock where
  getCurrentDir = do
    state <- get
    return $ currentPath state

  existsDirByAddress path = do
    state <- get
    let foundDir = findDirectory (splitPath path) [fs state]
    case foundDir of
      Nothing -> return False
      _ -> return True

  changeDirWithFullPath path = do
    prevState <- get
    put prevState {currentPath = path}

  existsFile path = do
    state <- get
    directory <- getDirectory path
    case findDirectory (splitPath directory) [fs state] of
      Nothing -> return False
      Just dir -> do
        case findFile (takeFileName path) dir of
          Nothing -> return False
          _ -> return True

  normalizeFilePath path = return $ simplifyWindows path

  makeDirByAddress path = do
    state <- get
    when (head (splitPath path) /= "/") $ throwError $ NoSuchDirectoryError path
    put state {fs = treeDirectoryCreation (tail $ splitPath path) (fs state)}

  readFileByAddress path = do
    state <- get
    case findFile path (fs state) of
      Just file -> return $ text file
      Nothing -> throwError $ NoSuchFileError path

treeDirectoryCreation :: [FilePath] -> MockFSTree -> MockFSTree
treeDirectoryCreation [] currentNode = currentNode
treeDirectoryCreation (currentDir : otherDirs) currentNode =
  case mbIndex of
    Nothing ->
      currentNode
        { subDirs =
            treeDirectoryCreation otherDirs (emptyDir currentDir) : subDirs currentNode
        }
    Just ind ->
      currentNode
        { subDirs =
            treeDirectoryCreation otherDirs (subD !! ind) : getAllExcept ind subD
        }
  where
    subD = subDirs currentNode
    names = map dirName subD
    mbIndex = elemIndex currentDir names
    getAllExcept i items = take i items ++ drop (1 + i) items

-- | Empty dir
emptyDir :: FilePath -> MockFSTree
emptyDir name =
  MockDir
    { dirName = name,
      subDirs = [],
      innerFiles = [],
      mdPermissions = "readable"
    }

-- | Check if Mock FS contains current directory
-- Check recursive, that each directory in list of FilePath
-- exists in Mock FS (starting from root)
-- if exists - return goal directory
findDirectory :: [FilePath] -> [MockFSTree] -> Maybe MockFSTree
findDirectory (last : _) [] = Nothing
findDirectory [curDirName] (curNode : others) =
  if dirName curNode == curDirName
    then return curNode
    else findDirectory [curDirName] others
findDirectory names@(curDirName : otherNames) (curNode : otherNodes) =
  if dirName curNode == curDirName
    then findDirectory otherNames (subDirs curNode)
    else findDirectory names otherNodes

-- | Check if directory contains file with given name.
-- If so, return this file
findFile :: FilePath -> MockFSTree -> Maybe MockFile
findFile searchName node = findInList $ innerFiles node
  where
    findInList [] = Nothing
    findInList (x : others)
      | name x == searchName = Just x
      | otherwise = findInList others
