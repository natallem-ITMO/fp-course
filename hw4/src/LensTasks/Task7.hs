{-# LANGUAGE RankNTypes #-}

module LensTasks.Task7
  ( cd,
    file,
    ls
  )
where

import Lens.Micro (Traversal', filtered, traversed, (^.))
import LensTasks.Task6 (FS, content, fileName, name)

-- | Focus subdirectory with given name.
cd :: FilePath -> Traversal' FS FS
cd filePath = content . traversed . filtered (\fs -> fs ^. name == filePath)

-- | Focus on names of all entities in directory
ls :: Traversal' FS FilePath
ls = content . traversed . name

-- | Focus on given file name if such name exists in directory
file :: FilePath -> Traversal' FS FilePath
file filePath = content . traversed . filtered (\fs -> fs ^. fileName == filePath) . name
