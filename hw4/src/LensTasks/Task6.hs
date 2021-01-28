{-# LANGUAGE ScopedTypeVariables #-}

module LensTasks.Task6
  ( FS,
    content,
    dirName,
    fileName,
    name,
    scanDirectory
  )
where

import Control.Exception (SomeException, catch)
import Control.Monad.Extra (forM, ifM)
import Lens.Micro (Lens', Traversal', lens)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.FilePath (joinPath, takeBaseName)

-- | Data structure for File System
data FS
  = Dir
      { _name :: FilePath,
        _contents :: [FS]
      }
  | File
      { _name :: FilePath
      }
  deriving (Show)

-- | Try to read directory and its content recursively by given
--   filepath. If no such directory exits, throws IOError.
--   If not enough permissions, creates FS with empty content.
scanDirectory :: FilePath -> IO FS
scanDirectory dir = do
  ifM
    (doesDirectoryExist dir)
    ( do
        list <-
          listDirectory dir
            `catch` (\(ex :: SomeException) -> return [])
        inner <-
          forM
            list
            ( \filepath -> do
                let curFilePath = joinPath [dir, filepath]
                ifM
                  (doesFileExist curFilePath)
                  (return $ File filepath)
                  (scanDirectory curFilePath)
            )
        return (Dir (takeBaseName dir) inner)
    )
    (ioError (userError "Not a directory"))

-- | Lens for name of all FS types (files and dirs)
name :: Lens' FS FilePath
name = lens _name (\fs newName -> fs {_name = newName})

-- | Traversal for content of directory
content:: Traversal' FS [FS]
content f dir@(Dir n c) = (\content' -> dir {_contents = content'}) <$> f c
content _ notDir = pure notDir

-- | Traversal for name of files
fileName :: Traversal' FS FilePath
fileName f file@(File n) = (\name' -> file {_name = name'}) <$> f n
fileName _ notFile = pure notFile

-- | Traversal for name of directory
dirName :: Traversal' FS FilePath
dirName f dir@(Dir n _) = (\name' -> dir {_name = name'}) <$> f n
dirName _ notDir = pure notDir
