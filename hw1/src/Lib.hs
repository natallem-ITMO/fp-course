{-# LANGUAGE GeneralizedNewtypeDeriving #
# LANGUAGE InstanceSigs #
# LANGUAGE KindSignatures #
# LANGUAGE RecordWildCards #
# LANGUAGE MultiParamTypeClasses #

module Lib where

import Data.List (nub)
import Prelude hiding (Either)


foo1 x = x + 10

someFunc :: IO ()
someFunc = putStrLn "someFunc Hello world"

type BadUser = (Int, String, String)

userFullId :: BadUser -> String
userFullId (uid, login, name) = show uid ++ ":" ++ login

type SwapPair a b = (a, b) -> (b, a)

type TripleList a = [(a, a, a)]

foo :: TripleList a -> a
foo [(a, b, c)] = a

data TrafficLight = Red | Yellow | Green
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

lightToStr :: TrafficLight -> String
lightToStr Red = "Red"
lightToStr Yellow = "Yellow"
lightToStr Green = "Green"

data User = MkUser Int String String

getUid :: User -> Int
getUid (MkUser uid _ _) = uid

getName :: User -> String
getName (MkUser _ name _) = name

--Constructor should shart from capital letter
users =
  [ MkUser 1 "Ivan" "124",
    MkUser 4 "Ivan" "124",
    MkUser 3 "Sergey" "124"
  ]

--Data.List.nub $ map getName users

data Point2D a = Point2D a a

pointToList :: Point2D a -> [a]
pointToList (Point2D x y) = [x, y]

maxCoord :: Point2D Int -> Int
maxCoord (Point2D x y) = max x y

data IntResult
  = Success Int
  | Fail String

--check :t Success / Fail
saveDiv :: Int -> Int -> IntResult
saveDiv _ 0 = Fail "Devision by zero"
saveDiv x y = Success $ x `div` y

showResult :: IntResult -> String
showResult (Success n) = "Result " ++ show n
showResult (Fail s) = "Fail " ++ s

data Vector a
  = Vector2D a a
  | Vector3D a a a

packVector :: Vector a -> [a]
packVector (Vector2D a b) = [a, b]
packVector (Vector3D a b c) = [a, b, c]

vecLen :: Vector Double -> Double
vecLen = sqrt . sum . map (^ 2) . packVector

--Data.List.sortOn fst [(53, "fs"), (4,"fd")]

data Maybe_ a = Nothing_ | Just_ a

maybeSecond :: [a] -> Maybe_ a
maybeSecond (_ : x : _) = Just_ x
maybeSecond _ = Nothing_

showMaybe :: (Show a) => Maybe_ a -> String
showMaybe (Just_ a) = "Just " ++ show a
showMaybe (Nothing_) = "Nothing"

data Either_ a b = Left_ a | Right_ b

eitherSecond :: [a] -> Either_ String a
eitherSecond [] = Left_ "list is empty"
eitherSecond [_] = Left_ "list contain only one element"
eitherSecond (_ : x : _) = Right_ x

data List a = Nil | Cons a (List a)

myList :: List Int
myList = Cons 4 (Cons 5 (Cons 4 Nil))

myMap :: (a -> b) -> (List a) -> List b
myMap f Nil = Nil
myMap f (Cons st t) = Cons (f st) (myMap f t)

--RECORD SYNTAX
data User2 = User2
  { uid :: Int,
    login :: String,
    password :: String --compile will produce constructor and getters for fields
  }
  deriving (Show)

Syntax sugar for
data User = User Int String String

login :: User -> String
login (User _ log _) = log

same for uid and for password


isIvan :: User2 -> Bool
isIvan user = login user == "Ivan"

ivan :: User2
ivan =
  User2
    { login = "Ivan",
      uid = 123,
      password = "fds"
    }

--RECORD FIELD PATTERN

isStepan :: User2 -> Bool
isStepan User2 {login = name} = name == "stepan"

isSergey :: User2 -> Bool
isSergey User2 {login = "Sergey"} = True
isSergey _ = False

--RECORD UPDATE SYNTAX
cloneIvan = ivan {login = "sergey"} -- change value of field of ivan

--OPERATOR RECORD FIELD
data R = R {(-->) :: Int -> Int}

r = R {(-->) = (+ 1)}

data R2 = R2 {(~->) :: Int -> Int -> Int}

g = R2 {(~->) = (+)}

--(g ~->) 4 5

--RECORD AND SUM TYPES(not safety)
data User3
  = JustUser {login3 :: String, juid :: Int}
  | Admin {login3 :: String, aid :: Int}

--error juid $ Admin "Vasya" 4

-- *** Exception: No match in record selector juid

--better
data UserField = UserField {loginUserField :: String, idUserField :: Int}

data AdminField = AdminField {loginAdminField :: String, idAdminField :: Int}

data SafeRecordUser
  = UserSafeRecord UserField
  | AdminSafeRecord AdminField

getUserSafeRecord :: SafeRecordUser -> Maybe UserField
getUserSafeRecord (UserSafeRecord x) = Just x
getUserSafeRecord (AdminSafeRecord x) = Nothing

--(if getUserSafeRecord without Maybe) loginUserField $ getUserSafeRecord $ (UserSafeRecord UserField{loginUserField = "dsf", idUserField = 45})
isSafeAdmin :: SafeRecordUser -> Bool
isSafeAdmin AdminSafeRecord {} = True
isSafeAdmin _ = False

-- isSafeAdmin $ (UserSafeRecord UserField {}) == warn + false
-- isSafeAdmin $ (AdminSafeRecord AdminField {}) == warn + true

--RECORD WILD CARDS
toUnsafeUser2 :: User2 -> String
--alias for object
toUnsafeUser2 user@User2 {uid = 0, ..} = "ROOT: " ++ login ++ ", " ++ password ++ " " ++ toUnsafeUser user
toUnsafeUser2 User2 {..} = login ++ ", " ++ password

toUnsafeUser :: User2 -> String
toUnsafeUser User2 {..} = "sfd"

newtype Message = Message String -- newtype = 1 constructor and 1 argument in constructor
--for what need it?

newtype SecretKey = SecretKey String

newtype PublicKey = PublicKey String

--derivePublicKey :: SecretKey -> PublicKey
--ckeckKeyPair :: (SecretKey, PublicKey) -> Bool
--ckeckKeyPair (secretKey, publicKey) = publicKey == derivePublicKey secretKey

--Ad-hoc polymorphism
class Printable p where
  printMe :: p -> String

data Foo = Foo | Bar
data Foo1 a  b = Foo1 a a

instance Printable Foo where
  printMe Foo = "Foo"
  printMe Bar = "Bar(whatever)"

helloMe :: Printable p => p -> String
helloMe p = "Hello, " ++ printMe p ++ "!"

instance Eq TrafficLight where
  (==) :: TrafficLight -> TrafficLight -> Bool
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  (==) _ _ = False

threeSame :: Eq a => a -> a -> a -> Bool
threeSame x y z = x == y && y == z

data Ordering = LT | EQ | GT

class Eq a => Ord a where
   compare              :: a -> a -> Ordering
   (<), (<=), (>=), (>) :: a -> a -> Bool

   compare x y
        | x == y    =  EQ
        | x <= y    =  LT
        | otherwise =  GT

   x <= y           =  compare x y /= GT
   x <  y           =  compare x y == LT
   x >= y           =  compare x y /= LT
   x >  y           =  compare x y == GT

class Num a where
      {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #

      (+), (-), (*)       :: a -> a -> a  -- self-explained
      negate              :: a -> a       -- unary negation
      abs                 :: a -> a       -- absolute value
      signum              :: a -> a       -- sign of number, abs x * signum x == x
      fromInteger         :: Integer -> a -- used for numeral literals polymorphism

      x - y               = x + negate y
      negate x            = 0 - x
When you write something like 7 it's just a syntax sugar for fromInteger 7. That's why numeric constants are polymorphic.
-}

boolTrue = read "True" :: Bool

--deriving
--minBound::TrafficLight => from Bounded // Red
--max Red Yellow => from Ord
--succ Red  -- pred Green => from Enum
--  toEnum 2 :: TrafficLight
--Green

newtype Size = Size Int
  deriving (Show, Read, Eq, Ord, Num)

--(Size 4) + (Size 3)
--Size 7

class Semigroup m where
      (<>) :: m -> m -> m
  Associativity law for Semigroup:
    1. (x <> y) <> z ≡ x <> (y <> z)

    1. (++)
    2. max/min

newtype Sum     a = Sum     { getSum     :: a }
newtype Product a = Product { getProduct :: a }
instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

instance Num a => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)

class Semigroup m => Monoid m where
      mempty :: m
  Identity laws for Monoid:
    2. x <> mempty ≡ x
    3. mempty <> x ≡ x
    ex - (++)



--Task 1
newtype Last a = Last {getLast :: Maybe a}
  deriving (Show)

instance Semigroup (Last a) where
  Last x <> Last Nothing = Last x
  Last _ <> Last (Just a) = Last (Just a)

instance Monoid (Last a) where
    mempty          = Last Nothing

--instance Last(Maybe b) a => Monoid a where
--   mempty :: Last (Maybe a)
--   mempty = Last (Maybe 3)


--Task 2
mconcat' :: Monoid m => [m] -> m
mconcat' [x] = x
mconcat' (x:xs) = x <> mconcat' xs

--Task 3
--Реализуйте функцию через foldr
--foldMap' :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
--foldMap' g a = foldr $ fmap g a(



-}
