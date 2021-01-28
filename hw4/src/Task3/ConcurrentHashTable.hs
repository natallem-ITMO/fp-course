module Task3.ConcurrentHashTable
  ( ConcurrentHashTable (..),
    newCHT,
    getCHT,
    putCHT,
    sizeCHT,
  )
where

import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    newTVar,
    readTVar,
    writeTVar,
  )
import Control.Concurrent.STM.TVar
  ( TVar,
    newTVar,
    readTVar,
    writeTVar,
  )
import Control.Monad.Extra (maybeM)
import Data.Hashable (Hashable (hash))
import Data.Vector (Vector, forM_, length, replicateM, (!))
import Prelude hiding (length, replicate)

-- | Type alias for element in ConcurrentHashTable
type MapElement k v = TVar (Maybe (k, v))

-- | Type alias for hash table in ConcurrentHashTable
type HashTable k v = Vector (MapElement k v)

-- | Data type for ConcurrentHashTable
data ConcurrentHashTable k v = ConcurrentHashTable {size :: TVar Int, hashPairs :: TVar (HashTable k v)}

-- | Initial size of ConcurrentHashTable, using by default
initSize :: Int
initSize = 30

-- | Returns new empty ConcurrentHashTable of default size
newCHT :: IO (ConcurrentHashTable k v)
newCHT = do
  atomically $ do
    initSizeTVar <- newTVar 0
    vec <- Data.Vector.replicateM initSize $ newTVar Nothing
    table <- newTVar vec
    return $ ConcurrentHashTable initSizeTVar table

-- | Try to get element value by key from ConcurrentHashTable
getCHT ::
  (Hashable k, Eq k) => 
  k ->                        -- ^ key of element to find in ConcurrentHashTable
  ConcurrentHashTable k v ->  -- ^ ConcurrentHashTable where find element
  IO (Maybe v)                -- ^ Returns Nothing in IO monad if ConcurrentHashTable doesn't contain element
                              --   Otherwise returns IO (Just <Value of element by given key>)
getCHT key hashTable = do
  atomically $ do
    table <- readTVar $ hashPairs hashTable
    curValue <- getMapElementForKey key table
    maybeM
      (return Nothing)
      (\(k, v) -> return $ Just v)
      (readTVar curValue)

-- | Put element with given key and value into ConcurrentHashTable
--   If element with such key already exists - replace it by new element
--   Otherwise add new (key,value) element in ConcurrentHashTable
putCHT :: (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value hashTable = do
  atomically $ do
    table <- readTVar $ hashPairs hashTable
    curSize <- readTVar $ size hashTable
    curValue <- getMapElementForKey key table
    maybeM
      ( do
          if needToRehash (curSize + 1) (length table)
            then do
              newVec <- rehash table (key, value)
              writeTVar (hashPairs hashTable) newVec
              writeTVar (size hashTable) (curSize + 1)
            else do
              writeTVar curValue $ Just (key, value)
              writeTVar (size hashTable) (curSize + 1)
      )
      (\_ -> writeTVar curValue $ Just (key, value))
      (readTVar curValue)

-- | Return current number of elements in ConcurrentHashTable
sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT hashTable = atomically $ do readTVar $ size hashTable

-- | Get element of ConcurrentHashTable by given key
--   If no element exists in given ConcurrentHashTable,\
--   returns element of ConcurrentHashTable where new element 
--   with such key should be placed
getMapElementForKey :: (Hashable k, Eq k) => k -> HashTable k v -> STM (MapElement k v)
getMapElementForKey key table = do
  plusInd 0
    where 
      l = length table
      start = hash key
      plusInd x = do
        let index = (start + x) `mod` l
        let curElem = table ! index
        maybeM
          (return curElem)
          ( \(k, v) ->
              if k == key
                then return curElem
                else plusInd (x + 1)
          )
          (readTVar curElem)

-- | Checks if fullness of ConcurrentHashTable is correct to protect from collisions 
needToRehash :: Int -> Int -> Bool
needToRehash curSize maxSize = fromIntegral maxSize * 0.75 < fromIntegral curSize

-- | Create bigger ConcurrentHashTable and move all elements from previous 
--   ConcurrentHashTable to new one. Returns new ConcurrentHashTable
rehash :: (Hashable k, Eq k) => HashTable k v -> (k, v) -> STM (HashTable k v)
rehash prevVector newElem = do
  newVector <- Data.Vector.replicateM (length prevVector * 2) $ newTVar Nothing
  let write (k, v) = do
        elem <- getMapElementForKey k newVector
        writeTVar elem $ Just (k, v)
  Data.Vector.forM_
    prevVector
    ( \elem -> do
        maybeM
          (return ())
          write
          (readTVar elem)
    )
  write newElem
  return newVector
