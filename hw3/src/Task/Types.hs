module Task.Types
  (
    FSError(..)
  , Command (..)
  , FileEntity(..)
  , DirEntity(..)
  ) where

import Control.Exception
import Data.Time.Clock (UTCTime)

-- | Error, that detecting while working with IO FS
data FSError
  = NoSuchDirectoryError String
  | NoSuchFileError String
  | AlreadyExistsDirectoryError String
  | AlreadyExistsFileError String
  | PermissionError String
  | IncorrectFileName String
  | OtherError {errorMessage :: String, path :: String}

instance Exception FSError

instance Show FSError where
  show (NoSuchDirectoryError err) = "FS ERROR: No such directory: \"" ++ err ++ "\""
  show (NoSuchFileError err) = "FS ERROR: No such file: \"" ++ err ++ "\""
  show (AlreadyExistsDirectoryError err) = "FS ERROR: Directory is already exist: \"" ++ err ++ "\""
  show (AlreadyExistsFileError err) = "FS ERROR: File is already exist: \"" ++ err ++ "\""
  show (PermissionError err) = "FS ERROR: No permissions for operatation with path: \"" ++ err ++ "\""
  show (IncorrectFileName name) = "FS ERROR: Incorrect file name: \"" ++ name ++ "\""
  show (OtherError e a) = "FS ERROR: Error " ++ e ++ ". Occured with path \"" ++ a ++ "\""


-- | Type of commands for user input.
data Command = HELP                  -- | usage guide
            |  CD FilePath           -- | go to directory
            |  DIR                   -- | show the content of current directory
            |  MKDIR FilePath        -- | create directory
            |  TOUCH FilePath        -- | create file
            |  CAT FilePath          -- | show the content of file
            |  RM FilePath           -- | remove file
            |  RMDIR FilePath        -- | remove directory
            |  GREP FilePath         -- | find file in current directory or subdirectories
            |  ECHO FilePath [String]-- | write text to file
            |  LS FilePath           -- | get file information
            |  STAT FilePath         -- | get directory information
                      deriving (Show)


-- | Representation of file with info fields.
data FileEntity = FileEntity
  {
    fePath :: FilePath,     -- | absolute file path
    fePermissions :: String,-- | file's permissions
    feSize :: Integer,      -- | size in bytes
    feType :: String,       -- | file type
    feTime :: UTCTime       -- | UTC-format time of creation
  }
  deriving (Show)

-- | Representation of directory with info fields.
data DirEntity = DirEntity
  {
    dePath :: FilePath,       -- | absolute directory path
    dePermissions :: String,  -- | directories permissions
    deFilesAmount :: Integer, -- | amount of first-level files and subdirectories
    deSize :: Integer         -- | size in bytes
  }
  deriving (Show)
