module Main where

import System.Directory (getCurrentDirectory)
import System.Directory.Internal.Prelude(tryIOError)
import Distribution.System

import Task.InitFS (initFS)

-- | Application entry point, takes path to initialize file-system in.
main :: IO ()
main = do
  case buildOS of
    Windows -> run_console
    Linux -> run_console 
    OSX -> run_console
    _ -> putStrLn "You OS is not supported"
  where 
    run_console = do
      putStrLn
        "Enter the FilePath you want to initialize the file-system in (leave empty to init in current directory)"
      path <- getLine
      if path == ""
        then do
          try <- tryIOError getCurrentDirectory
          case try of
            Right curPath -> initFS curPath
            Left error -> putStrLn $ "Couldn't obtain current directory:" ++ show error
        else initFS path
