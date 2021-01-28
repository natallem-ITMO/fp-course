-------------------------------------
-- Lecture 08: Speeding up Haskell --
-------------------------------------
 
-- Difference list
 
newtype DifferenceList a = DL { unDL :: [a] -> [a] }
 
fromList :: [a] -> DifferenceList a
fromList = DL . (++)
 
toList :: DifferenceList a -> [a]
toList (DL f) = f []
 
instance Monoid (DifferenceList a) where
    DL f <> DL g = DL $ f . g
    
-- Strict evaluation (feat. irrefutable patterns)
 
seq :: a -> b -> b
seq _|_ b = _|_
seq _   b = b
 
data    DataWrapper    a = DW a
newtype NewtypeWrapper a = NW a
 
ghci> DW undefined `seq` 22
22
ghci> NW undefined `seq` 22
*** Exception: Prelude.undefined
 
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f a []     = a
foldlStrict f a (x:xs) = let aa = f a x
                         in seq aa (foldlStrict f aa xs)
                    
class NFData a where  -- Normal Form Data
    rnf :: a -> ()
    rnf a = a `seq` ()
    
deepseq :: NFData a => a -> b -> b
a `deepseq` b = rnf a `seq` b
 
f !a  = ... ≡ f a | a `seq` False = undefined; f a = ... -- to transform the argument into its weak head normal form
 
($!) :: (a -> b) -> a -> b                 -- strict function application
f $! x = let !vx = x in f vx
 
($!!) :: NFData a => (a -> b) -> a -> b  -- deep strict function application
f $!! x = x `deepseq` f x
 
f :: (a, b) -> Int
f (a, b) = const 1 a   -- pair pattern is too strict
 
g :: (a, b) -> Int
g ~(a, b) = const 1 a  -- irrefutable pattern ≡ lazy pattern
 
f1 :: Either e Int -> Int
f1 ~(Right 1) = 42
 
ghci > f (Left "kek")
42
ghci > f (error "mda")
42
 
-- Stream fusion
 
data Step s a = Done 
              | Skip    s 
              | Yield a s
 
data Stream a = forall s . Stream (s -> Step s a) s
 
stream :: forall a . [a] -> Stream a
stream xs = Stream next xs 
  where
    next :: [a] -> Step [a] a
    next []     = Done
    next (x:xs) = Yield x xs
    
unstream :: forall a . Stream a -> [a]
unstream (Stream next s0) = go s0 
  where
    go s = case next s of 
             Done       -> []
             Skip ss    -> go ss
             Yield a ss -> a : go ss
             
mapS :: forall a b . (a -> b) -> Stream a -> Stream b
mapS f (Stream next s) = Stream nextt s 
  where
    nextt xs = case next xs of 
                    Done       -> Done
                    Skip ss    -> Skip ss
                    Yield a ss -> Yield (f a) ss
 
filterS :: forall a . (a -> Bool) -> Stream a -> Stream a
filterS p (Stream next s) = Stream nextt s 
  where
    nextt xs = case next xs of 
                    Done       -> Done
                    Skip ss    -> Skip ss
                    Yield a ss -> if p a then Yield a ss else Skip ss
                    
-- Mutable objects innit
 
data ST s a  -- The strict state-transformer monad
 
runState :: State s a -> s -> (a, s)  -- use evalState with state to get result
runST    :: (forall s. ST s a) -> a   -- forall trick
 
data STRef s a  -- a mutable variable
 
newSTRef    :: a -> ST s (STRef s a) 
readSTRef   :: STRef s a -> ST s a
writeSTRef  :: STRef s a -> a -> ST s ()
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
 
class Monad m => MArray a e m where  -- type class for all arrays
 
data STArray s i e :: * -> * -> * -> *  -- Mutable, boxed, non-strict arrays
-- s: the state variable argument for the ST type
-- i: the index type of the array (should be an instance of Ix), usually Int
-- e: the element type of the array.
 
data STUArray s i e  -- A mutable array with unboxed elements 
                     -- (Int, Double, Bool, etc.)
                     
newArray   :: Ix i => (i, i) -> e -> m (a i e)
readArray  :: (MArray a e m, Ix i) => a i e -> i -> m e
writeArray :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
 
data Vector    a  -- immutable vectors
data MVector s a  -- mutable vectors
 
-- immutable vectors
(!) :: Vector a -> Int -> a  -- O(1) indexing
fromList :: [a] -> Vector a
-- map, filter, etc. 
 
-- mutable vectors
read   :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
write  :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()
grow   :: PrimMonad m => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
freeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
 
---------------------------------------------------------
-- Lecture 09: Concurrency, Exceptions and parallelism --
---------------------------------------------------------
 
-- forking a thread
 
forkIO :: IO () -> IO ThreadId  -- creates lightweight thread
 
import Control.Concurrent
 
main = do
  _threadId <- forkIO $ do
    threadDelay 1000000
    putStrLn "Forked thread awake"
  threadDelay 2000000
  putStrLn "Main thread finishes"
 
-- Mutex variable
 
data MVar a  -- empty or full box; mutex variable
 
newEmptyMVar :: IO (MVar a)           -- create empty box
putMVar      :: MVar a -> a -> IO ()  -- fill box with value
takeMVar     :: MVar a -> IO a        -- take var with block
 
import Control.Concurrent
 
main = do
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
 
-- K i l l i n g t h r e a d s (figuratively speaking) and catching Exceptions
 
throwTo :: Exception e => ThreadId -> e -> IO ()
 
killThread :: ThreadId -> IO ()
killThread tid = throwTo tid ThreadKilled
 
throwIO :: Exception e => e -> IO a
throw :: Exception e => e -> a
 
catch :: Exception e => IO a -> (e -> IO a) -> IO a
 
handle :: Exception e => (e -> IO a) -> IO a -> IO a
handle = flip catch
 
mask_ :: IO a -> IO a -- Executes an IO computation with asynchronous exceptions masked. I.e. any thread which attempts to raise an exception in the current thread with throwTo will be blocked until asynchronous exceptions are unmasked again.
 
-- bracket reminder + finally
 
bracket :: IO a        -- computation to run first, acquiring the resource
        -> (a -> IO b) -- computation to run last, releasing the resource
        -> (a -> IO c) -- main computation to run in-between 
        -> IO C
        
finally :: IO a -- computation to run first
        -> IO b -- computation to run afterward
        -> IO a
        
-- Control.Concurrent.Async
 
withAsync       :: IO a -> (Async a -> IO b) -> IO b
wait            :: Async a -> IO a
cancel          :: Async a -> IO ()
concurrently    :: IO a -> IO b -> IO (a, b)
race            :: IO a -> IO b -> IO (Either a b)
mapConcurrently :: Traversable t => (a -> IO b) -> t a -> IO (t b)
 
-- Software Transactional Memory
 
data STM a  -- software transactional memory
instance Monad STM
 
atomically :: STM a -> IO a
 
data TVar a -- transactional variable
newTVar   :: a -> STM (TVar a)
readTVar  :: TVar a -> STM a
writeTVar :: TVar a -> a -> STM ()
 
retry     :: STM a                   -- try again current transaction
orElse    :: STM a -> STM a -> STM a -- if first retries then call second
 
throwSTM  :: Exception e => e -> STM a
catchSTM  :: Exception e => STM a -> (e -> STM a) -> STM a
 
-- \/ example \/
 
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
    
-- Eval
 
data Eval a -- Eval is monad for parallel computation
instance Monad Eval
 
runEval :: Eval a -> a  -- pull the result out of the monad
 
rpar :: a -> Eval a  -- suggest to parallel, create *spark* 
rseq :: a -> Eval a  -- wait for evaluation of argument (eval it to WHNF)
 
-------------------------------------------------
-- Lecture 10: Lens... but no Template Haskell --
-------------------------------------------------
 
-- Lens
 
type Lens  s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
type Lens' s   a   = Lens s s a a
 
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \f s -> set s <$> f (get s)
 
newtype Const a x = Const { getConst :: a }
instance Functor (Const a) where
    fmap _ (Const v) = Const v
 
view :: Lens' s a -> s -> a
view l s = getConst $ l Const s
 
newtype Identity a = Identity { runIdentity :: a }
instance Functor Identity where
    fmap f (Identity x) = Identity (f x)
    
over :: Lens' s a -> (a -> a) -> s -> s
over l fn s = runIdentity $ l (Identity . fn) s
 
set :: Lens' s a -> a -> s -> s
set l a s = runIdentity $ l (Identity . const a) s
 
-- Lens laws
 
1. view l (set l field obj)      ≡ field
2. set l (view l obj) obj        ≡ obj
3. set l field (set l field obj) ≡ set l field obj
 
-- operators
 
(.~) :: Lens' s a -> a        -> (s -> s)
(.=) :: Lens' s a -> a        -> State s ()
 
(%~) :: Lens' s a -> (a -> a) -> (s -> s)
(%=) :: Lens' s a -> (a -> a) -> State s ()
 
(^.) :: s -> Lens' s a -> a
 
-- Traversal
 
type Traversal  s t a b = forall f . Applicative f => (a -> f b) -> (s -> f t)
type Traversal' s   a   = forall f . Applicative f => (a -> f a) -> (s -> f s)
 
-- Getting
 
type Getting r s a = (a -> Const r a) -> s -> Const r s
 
toListOf :: Getting (Endo [a]) s a -> s -> [a]
(^..)    :: s -> Getting (Endo [a]) s a -> [a] 
 
-- Prism
 
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
    
preview :: Prism' s a -> s -> Maybe a
review :: Prism' s a -> a -> s
_Left :: Prism' (Either a b) a
_Just :: Prism' (Maybe a) a
_Cons :: Prism' [a] (a, [a])
_Nil :: Prism' [a] ()
 
-- \/ Prism examples \/
 
ghci> preview _Left (Left "hi")
Just "hi"
ghci> preview _Left (Right "hi")
Nothing
 
ghci> review _Left "hi"
Left "hi"
 
ghci> preview _Cons []
Nothing
 
ghci> preview _Cons [1,2,3]
Just (1, [2,3])
 
ghci> preview _Nil []
Just ()
 
ghci> preview _Nil [1,2,3]
Nothing
 
-------------------------------------
-- Lecture 11: Brand new DSL world --
-------------------------------------
 
-- GADT example
 
data ArithExpr a where
  AENum  :: Int -> ArithExpr Int
  AEPlus :: ArithExpr Int -> ArithExpr Int -> ArithExpr Int
  AEAnd  :: ArithExpr Bool -> ArithExpr Bool -> ArithExpr Bool
  AEGt   :: ArithExpr Int -> ArithExpr Int -> ArithExpr Bool
 
-- Existential types, type equality & type application
 
data SomeAE where
  SomeAE :: (Typeable a, Show a) => ArithExpr a -> SomeAE
 
-- | The class 'Typeable' allows
-- a concrete representation of
-- a type to be calculated.
class Typeable (a :: k)
 
-- | Propositional equality.
-- If @a :~: b@ is inhabited by some
-- terminating value, then the type @a@
-- is the same as the type @b@.
data a :~: b where
  Refl :: a :~: a
 
-- | Extract a witness of equality
-- of two types
eqT
  :: forall a b. (Typeable a, Typeable b)
  => Maybe (a :~: b)
  
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
 
parseInt
  :: String -> Maybe (ArithExpr Int)
parseInt s = parse s >>=
  \(SomeAE (expr :: ArithExpr t)) ->
    do
      Refl <- eqT @t @Int
      pure expr
 
-- Universal quantifier & rank-N types
 
length :: forall a . [a] -> Int
 
{-# LANGUAGE RankNTypes #-}
 
applyToTuple :: (forall a. [a] -> Int) -> ([b], [c]) -> (Int, Int)
applyToTuple f (x, y) = (f x, f y)
 
Rank 0: Int
Rank 1: forall a . a -> Int
Rank 2: (forall a . a -> Int) -> Int
Rank 3: ((forall a . a -> Int) -> Int) -> Int
 
-- ST, once again, ffs
 
runST :: forall α. (forall s. ST s α) -> α
 
newSTRef :: forall α s. α -> ST s (STRef s α)
readSTRef :: forall α s. STRef s α -> ST s α
writeSTRef :: forall α s. STRef s α -> α -> ST s ()
 
-- Scoped type variables
 
{-# LANGUAGE ScopedTypeVariables #-}
 
calc3 :: forall a. Num a => a -> a -> a
calc3 a b = a + f b
  where
    f :: a -> a
    f = (+ 10)
    
-- TAGLESS FINAL
 
class ArithExpr expr where
  aeNum  :: Int -> expr Int
  aePlus :: expr Int -> expr Int -> expr Int
  aeAnd  :: expr Bool -> expr Bool -> expr Bool
  aeGt   :: expr Int -> expr Int -> expr Bool
 
newtype Interpret a =
  Interpret { interpret :: a }
 
instance ArithExpr Interpret where
  aeNum = Interpret
  aePlus a b = Interpret $
    interpret a + interpret b
  aeAnd a b = Interpret $
    interpret a && interpret b
  aeGt a b = Interpret $
    interpret a > interpret b
 
-------------------------------------
-- Lecture 12: Some fun with kinds --
-------------------------------------
 
-- ConstraintKinds
 
{-# LANGUAGE ConstraintKinds #-}
 
type MyConstraints a = (Read a, Num a, Show a)
 
foo :: MyConstraints a => String -> a -> a
 
-- Datatype promotion
 
{-# LANGUAGE DataKinds #-}
 
data Nat = Z | S Nat
 
data Vec :: * -> Nat -> * where
    Nil  :: Vec a Z
    Cons :: a -> Vec a n -> Vec a (S n)
    
zipV :: Vec a n -> Vec b n -> Vec (a, b) n
zipV       Nil       Nil = Nil
zipV (x :> xs) (y :> ys) = (x, y) :> zipV xs ys
 
-- Heterogenous lists
 
data HList :: [*] -> * where
    HNil :: HList '[]
    (:^) :: a -> HList t -> HList (a ': t)
 
infixr 2 :^
 
-- Instantiating such data types
 
instance Show (HList '[]) where
    show _ = "H[]"
    
instance (Show e, Show (HList l)) => Show (HList (e ': l)) where
    show (x :^ l) = let 'H':'[':s = show l
                    in "H[" ++ show x ++ (if s == "]" then s else ", " ++ s)
                    
-- Type families
 
type family Foo bar :: * where
  Foo Char = Double
  Foo b = b
 
type family Foo bar :: *
type instance Foo Char = Double
type instance Foo Int = Int
 
-- Free monad
 
data Free f a = Pure a | Free (f (Free f a)) -- Free monad is an abstraction of multistep computation where each subsequent step requires some input from the outer context to continue. 
 
instance Functor f => Monad (Free f) where
  return = Pure
 
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)
 
--------------------------
-- Lecture 13: Comonads --
--------------------------
 
-- Comonad
 
class Functor w => Comonad w where
    extract   :: w a -> a
    duplicate :: w a -> w (w a)           -- extend id x
    extend    :: (w a -> b) -> w a -> w b -- fmap f <$> duplicate x
 
-- 'extend' in operator form with arguments flipped
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
 
-- Zippers
 
Zipper(f, a) = a * f'(a)
 
List(a) = 1 + a * List(a) = 1 / (1 - a)
List'(a) = (1 / (1 - a)) ^ 2 = List(a) * List(a)
Zipper(List, a) = List(a) * a * List(a)
 
data ListZipper a = LZ [a] a [a] deriving Functor  -- allows to focus on a single element
 
instance Comonad ListZipper where
    extract :: ListZipper a -> a
    extract (LZ _ x _) = x
    
    extend :: (ListZipper a -> b) -> ListZipper a -> ListZipper b
    extend f lz@(LZ l a s) = LZ (goListL f lz) (f lz) (goListR f lz)
      where
        goListL :: (ListZipper a -> b) -> ListZipper a -> [b]
        goListL f lz@(LZ [] _ _) = []
        goListL f lz@(LZ (x : xs) a s) = f (moveLL lz) : goListL f (moveLL lz)
 
        goListR :: (ListZipper a -> b) -> ListZipper a -> [b]
        goListR f lz@(LZ _ _ []) = []
        goListR f lz@(LZ l a (x : xs)) = f (moveLR lz) : goListR f (moveLR lz)
 
data Tree a = Nil | Node (Tree a) a (Tree a) deriving Functor
 
Tree(a) = 1 + a * Tree^2(a)
Tree'(a) = Tree^2(a) + 2 * a * Tree(a) * Tree'(a) = Tree^2(a) / (1 - 2 * a * Tree(a)) = Tree^2(a) * List(2 * a * Tree(a))
Zipper(Tree, a) = Tree(a) * a * Tree(a) * List(a * Tree(a) + a * Tree(a))
 
data Branch a = LB (BinTree a) a | RB a (BinTree a) deriving Functor
data BinTreeZipper a = BTZ (BinTree a) a (BinTree a) [Branch a] deriving Functor
 
-- Env, Coreader
 
data Env e a = Env e a
 
instance Comonad (Env e) where
    extract :: Env e a -> a
    extract (Env _ a) = a
 
    extend :: (Env e a -> b) -> Env e a -> Env e b
    extend f env@(Env e _) = Env e (f env)
    
-- Traced, Cowriter
 
newtype Traced m a = Traced { runTraced :: m -> a }
 
instance Monoid m => Comonad (Traced m) where
    extract :: Traced m a -> a
    extract  (Traced ma) = ma mempty
 
    extend :: (Traced m a -> b) -> Traced m a -> Traced m b
    extend f (Traced ma) = Traced $ \m -> f (Traced $ \m' -> ma (m <> m'))
    
-- Store, Costate
 
data Store s a = Store (s -> a) s deriving Functor
 
instance Comonad (Store s) where
    extract :: Store s a -> a
    extract  (Store f s) = f s
 
    extend :: (Store s a -> b) -> Store s a -> Store s b
    extend f (Store g s) = Store (f . Store g) s 
 
-- codo-notation... one of the many...
 
next123 :: Iterator a -> [a]
next123 = method
        this & next
    i1> this & next
    i2> this & next
    i3> [i1  & extract, i2 & extract, i3 & extract]
    
next123 =
      \_i0 ->
    let i1 =      extend (\this -> this & next) _i0
        i2 =      extend (\this -> this & next)  i1
        i3 =      extend (\this -> this & next)  i2
     in extract $ extend (\this ->
            [i1 & extract, i2 & extract, i3 & extract]) i3
RAW Paste Data