{-# LANGUAGE ScopedTypeVariables #-}
module Task.Invoke
  ( invoke
  ) where
  
import Data.IORef (IORef)
import Data.List(intercalate)
import Control.Monad.Catch(try)
import Control.Monad.Reader(ReaderT, liftIO)

import Task.FSAction(FsAction(..))
import Task.Utils(helpText)
import Task.Types( Command(..), DirEntity(..), FSError(..), FileEntity(..))

-- | Invokes parsed command and prints the output.
invoke :: Command  -> ReaderT (IORef FilePath) IO String
invoke HELP = do  
  liftIO $ return helpText

invoke (CD fileName) = do
  (res :: Either FSError ()) <- try (cd fileName)
  case res of 
    Right _ -> return ""
    Left error -> return $ show error

invoke (MKDIR fileName) = do
  (res :: Either FSError ()) <- try (mkdir fileName)
  case res of
    Right _ -> return ""
    Left error -> return $ show error

invoke DIR = do
  (res :: Either FSError [String]) <- try dir
  case res of
    Right dirs -> return $ "\t" ++  intercalate "\n\t" dirs
    Left error -> return $ show error

invoke (TOUCH path) = do
  (res :: Either FSError ()) <- try $ touch path
  case res of
    Right dirs -> return ""
    Left error -> return $ show error

invoke (CAT path) = do
  (res :: Either FSError String) <- try $ cat path
  case res of
    Right text -> return text
    Left error -> return $ show error

invoke (RM path) = do
  (res :: Either FSError ()) <- try $ rm path
  case res of
    Right () -> return ""
    Left error -> return $ show error

invoke (RMDIR path) = do
  (res :: Either FSError ()) <- try $ rmdir path
  case res of
    Right () -> return ""
    Left error -> return $ show error

invoke (GREP path) = do
  (res :: Either FSError (Maybe String)) <- try $ grep path
  case res of
    Right (Just path) -> return $ "found file in " ++ path
    Right Nothing -> return "file not found"
    Left error -> return $ show error

invoke (ECHO path text) = do
  (res :: Either FSError ()) <- try $ echo path (unwords text)
  case res of
    Right () -> return ""
    Left error -> return $ show error

invoke (LS path) = do
  (res :: Either FSError FileEntity) <- try $ ls path
  case res of
    Right entity -> return $ show entity
    Left error -> return $ show error

invoke (STAT path) = do
  (res :: Either FSError DirEntity) <- try $ stat path
  case res of
    Right entity -> return $ show entity
    Left error -> return $ show error
