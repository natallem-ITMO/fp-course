module Task.Utils
  ( normalizeFilePathIO,
    helpText,
  )
where

import System.Directory (makeAbsolute)
import System.Directory.Internal (simplifyWindows)

-- | Simplify given path for clear
--  representation in command line
normalizeFilePathIO :: FilePath -> IO FilePath
normalizeFilePathIO path = do
  absPath <- makeAbsolute path
  return (simplifyWindows absPath) -- use Windows simplify to expand (..)

-- | Custom helper table.
helpText :: String
helpText =
  "Available commands:\n\n"
    ++ "    help                   -- ^ show usage guide\n"
    ++ "    cd    <directory>      -- ^ go to directory (use \"cd ..\" to go to the parent directory)\n"
    ++ "    dir                    -- ^ show the content of current directory\n"
    ++ "    mkdir <directory-name> -- ^ create new directory and subdirectories by given path\n"
    ++ "    touch <file-name>      -- ^ create file with given name by given path\n"
    ++ "    cat   <file>           -- ^ show the content of file\n"
    ++ "    rm    <file>           -- ^ delete file by given path\n"
    ++ "    rmdir <directory>      -- ^ delete directory and it's content\n"
    ++ "    grep  <file>           -- ^ find file in current directory or subdirectories\n"
    ++ "    echo  <file> <text>    -- ^ append text to file\n"
    ++ "    ls    <file>           -- ^ get file information\n"
    ++ "    stat  <directory>      -- ^ get directory information\n\n"
