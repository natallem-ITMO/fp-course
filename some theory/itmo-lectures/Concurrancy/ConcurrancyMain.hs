module Main where
import Control.Concurrent 
import Control.Concurrent.Async
import Control.Exception
import Control.Monad 
import Control.Concurrent.STM

main :: IO ()
main = do
  -- parallelMVar
  handleAsyncException
  putStrLn "String" 



parallelWithConcurrently :: IO () 
parallelWithConcurrently = do 
  (r1, r2) <- concurrently aaa bbb
  return ()
    where
      aaa = do
        threadDelay 1000000 
        putStrLn "hello 1 sec"
        aaa
      bbb = do
        threadDelay 3000000 
        putStrLn "hello 3 sec"
        bbb

parallelForkIO :: IO ()
parallelForkIO = do 
  _threadId <- forkIO $ do
    foo 
  _threadId2 <- forkIO $ do
    foo1 
  threadDelay 300000000 
  putStrLn "Main thread finished"
  where 
    foo :: IO () 
    foo = do 
      threadDelay 1000000 
      putStrLn "hello 1 sec"
      foo
    foo1 :: IO () 
    foo1 = do 
      threadDelay 3000000 
      putStrLn "hello 3 sec"
      foo1



parallelMVar :: IO () 
parallelMVar = do
  tm1 <- newEmptyMVar
  tm2 <- newEmptyMVar
  _threadId1 <- forkIO $ do
    threadDelay 1000000
    putMVar tm1 100500
  _threadId2 <- forkIO $ do
    threadDelay 1000000
    putMVar tm2 "This is horosho"
  r1 <- takeMVar tm1
  r2 <- takeMVar tm2
  putStrLn $ "r1: " <> show r1 <> ", r2: " <> show r2


handleAsyncException :: IO ()
handleAsyncException = do 
  handle intrHandler $
    forM_ [1..1000] $ \i -> do
      threadDelay 1000000
      putStrLn $ "Finished round " <> show i

intrHandler :: AsyncException -> IO ()
intrHandler UserInterrupt = putStrLn "Finishing due to user interrupt ..."
intrHandler e = putStrLn $ "Caught async exception: " <> show e


worker :: Int -> IO Int  -- simulate some work
worker n = threadDelay (10^2 * n) >> return (n * n)
-- Spawn 2 threads in parallel, halt on both finished.
test1 :: IO (Int, Int)
test1 = concurrently (worker 1000) (worker 2000)
-- Spawn 2 threads in parallel, halt on first finished.
test2 :: IO (Either Int Int)
test2 = race (worker 1000) (worker 2000)

-- Spawn 10000 threads in parallel, halt on all finished.
test3 :: IO [Int]
test3 = mapConcurrently worker [0..10000]

type Account = TVar Integer

credit :: Integer -> Account -> STM ()
credit amount account = do
    current <- readTVar account
    writeTVar account (current + amount)

debit :: Integer -> Account -> STM ()
debit amount account = do
    current <- readTVar account
    writeTVar account (current - amount)

transfer :: Integer -> Account -> Account -> STM ()
transfer amount from to = do
    debit amount from
    credit amount to
