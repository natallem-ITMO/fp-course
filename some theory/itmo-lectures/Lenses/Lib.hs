{-# LANGUAGE Rank2Types       #-}
module Lenses.Lib
where
import Data.Functor.Identity ( Identity(Identity, runIdentity) )
import Data.Functor.Const ( Const(Const, getConst))

someFunc :: IO ()
someFunc = putStrLn "hello natasha"


-- type Lens  s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
-- type Lens' s a = Lens s s a a 
-- -- type Lens' s a = forall f. Functor f => (a -> f a) -> (s -> f s)

-- lens :: (s -> a ) -> (s -> a -> s) -> Lens' s a 
-- lens get set = \ f1 s -> set s <$> f1 (get s)


-- view :: Lens' s a -> s -> a
-- view l s = getConst $ l Const s 

-- over :: Lens' s a -> (a -> a) -> s -> s
-- over l f s = runIdentity $ l (Identity . f) s

-- newtype Person = Person{name :: String}

-- personLens:: Lens' Person String 
-- personLens = lens name $ \p a -> p{name = a}

-- ex :: String
-- ex = view personLens $ Person "Nata"

-- func ::  Int -> String
-- func int = show int

-- res :: Const Integer String
-- res = fmap func (Const 3)



