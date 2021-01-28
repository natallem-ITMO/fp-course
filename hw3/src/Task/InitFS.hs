module Task.InitFS
 (initFS)
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.IORef (IORef (..), newIORef, readIORef)
import Options.Applicative (execParserPure, getParseResult, prefs, showHelpOnEmpty)
import System.Directory (doesDirectoryExist)
import System.IO (hFlush, stdout)


import Task.Invoke(invoke)
import Task.Parser(opts)
import Task.Utils(helpText, normalizeFilePathIO)

-- | Run console FS : initialize FS and 
--  read and execute requests in infinite loop
initFS :: FilePath -> IO ()
initFS path = do
  fs <- doesDirectoryExist path
  if fs
    then do
      fullPath <- normalizeFilePathIO path
      ioRefPath <- newIORef fullPath
      runReaderT infiniteInput ioRefPath
    else do
      putStrLn "Cannot open initial directory"
      return ()

ifExistsEntry :: FilePath -> IO (Maybe FilePath)
ifExistsEntry entry = do
  isDir <- doesDirectoryExist entry
  if isDir
    then return (Just entry)
    else return Nothing

infiniteInput :: ReaderT (IORef FilePath) IO ()
infiniteInput = do
  ref <- ask
  curDir <- liftIO (readIORef ref)
  liftIO $ putStr $ curDir ++ ">"
  _ <- liftIO $ hFlush stdout
  input <- liftIO getLine
  case getParseResult $ execParserPure (prefs showHelpOnEmpty) opts (words input) of
    Just result -> do
      invokeResult <- invoke result
      liftIO $ putStrLn invokeResult
    Nothing -> do
      liftIO $
        putStrLn
          "Error on user input. See user guide."
      liftIO $ putStrLn helpText
  infiniteInput
