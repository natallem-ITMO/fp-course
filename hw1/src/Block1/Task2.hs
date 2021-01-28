{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
  (
    Nat (..)
  , divNat
  , isEven
  , modNat
  , natToInteger
  ) where

-- | Natural numbers representation.
--  Show representation is standard.
data Nat = Z | S Nat
  deriving (Show)

-- | Return previous Nat or Z if num is Z.
prevNat :: Nat -> Nat
prevNat Z = Z
prevNat (S n) = n

-- | Return sum of two given Nat numbers.
add :: Nat -> Nat -> Nat
add x Z = x
add x y = add (S x) (prevNat y)

-- | Return difference between two given Nat numbers.
sub :: Nat -> Nat -> Nat
sub x Z = x
sub x y = sub (prevNat x) (prevNat y)

-- | Multiply two given Nat numbers.
mul :: Nat -> Nat -> Nat
mul x (S Z) = x
mul _ Z = Z
mul x y = add x $ mul x $ prevNat y

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Z Z = True
  (==) _ Z = False
  (==) Z _ = False
  (==) x y = (==) (prevNat x) (prevNat y)

instance Ord Nat where
  compare :: Nat -> Nat -> Ordering
  compare Z Z = EQ
  compare Z _ = LT
  compare _ Z = GT
  compare x y = compare (prevNat x) (prevNat y)

instance Num Nat where
  (+) = add
  (-) = sub
  (*) = mul
  negate _ = Z
  abs = id
  signum Z = Z
  signum _ = S Z
  fromInteger x
    | x < 0 = error ("Cannot transform " ++ show x ++ " to Nat due to its nagetivity")
    | x == 0 = Z
    | otherwise = S (fromInteger (x - 1))

-- | Return Integer number corresponds given Nat number
natToInteger :: Nat -> Integer
natToInteger Z = 0
natToInteger x = (+) 1 $ natToInteger $ prevNat x


-- | Analog div for Nat
divNat :: Nat -> Nat -> Nat
divNat a b = fst (divModNat a b)

-- | Analog mod for Nat
modNat :: Nat -> Nat -> Nat
modNat a b = snd (divModNat a b)

-- | Find div and mod for Nat by subtract
divModNat :: Nat -> Nat -> (Nat, Nat)
divModNat x y = subUntilZero x y Z
  where
    subUntilZero a b acc
      | b == Z = error("Cannot divide by Zero")
      | a >= b = subUntilZero (a-b) b (S (acc))
      | otherwise = (acc, a)

-- | Check if given Nat number is even
isEven :: Nat -> Bool
isEven Z = True
isEven (S n) = not $ isEven n
