module D_standart_type_classes where
{-
Class extend (Расширение классов типов)
Похоже на наследование в ООП языках(наследование интрефейсов)

class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)

class (Eq a) => Ord a where // расширение классов типов : Ord расширяет класс Eq
  (<), (<=), (>=), (>) :: a -> a -> Bool
  min, max :: a -> a -> a
  compare :: a -> a -> Ordering

  Minimal complete definition : either compare or <=

  :i Ordering
  -data Ordering = LT | EQ | GT    -- Defined in `GHC.Types'//перечисление из 3 элементов
   instance Bounded Ordering -- Defined in `GHC.Enum'
   instance Enum Ordering -- Defined in `GHC.Enum'
   instance Eq Ordering -- Defined in `GHC.Classes'
   instance Ord Ordering -- Defined in `GHC.Classes'
   instance Read Ordering -- Defined in `GHC.Read'
   instance Show Ordering -- Defined in `GHC.Show'
-}

class (Eq a, Ord a) => MyClass a where -- наследование интерфейсов, а не реализаций, проблем с множественным наследованием не возникает


{-
Show - класс типов, с большой буквы
show - главная фукнция


show полиморфна по возвращаемому типу
    :t show
    - show :: Show t => t -> String

В Show много разных функций, это для эффективности

  show 5.5
  - "5.5"

  show "dsf"
  - "dsf"

Read - класс типов для чтения.
Вне этого класса определена фукнция read (внешняя вспомогательная функция)

  :t read
  - read :: Read a => String -> a

  read "5"
  ошибка из-за ambiguous

  read "5" :: Double
  - 5.0

  read "[3,1]" :: Double
  - [3.0, 1.0]

read потребляет весь ввод
  read "5 rings" :: Int
  -*** Exception: Prelude.read: no parse

reads позволяет считать парсить символы и хвост

  reads "5 rings" :: [(Int, String)] // все что в начале строки может быть интерпретированно как Int записывается, остальное в хвост
  -[(5," rings")]

Возвращаем список, т.к. можем вернуть
[element] - если разбор прошел хорошо
[], если произошла неудача в разборе
несколько элементов - неоднозначный разбор
-}


{-
Многие встроенные типы - типы перечисления (Bool = False, True). На них есть естественный порядок (Int, Char и т.д.)
Рассмотрим интерфейс Enum
class Enum a where
  succ, pred :: a -> a //successor and predecessor
  toEnum :: Int -> a // номер элемента
  fromEnum :: a -> Int

  toEnum 4 :: Double
  - 4.0

  succ True
  -*** Exception: Prelude.Enum.Bool.succ: bad argument // не определена для наибольшего значения

class Bound a where
  maxBound, minBound a // наибольшее наименьшее значние

Единственный тип, который Enum but not Bound is Integer


class (Bounded a, Enum a) => SafeEnum a where
  ssucc :: a -> a
  ssucc = \t -> if (t == (maxBound :: a)) then return (minBound::a) else return succ t

  spred :: a -> a
  spred = \t -> if (t == (minBound :: a)) then return (maxBound::a) else return pred t
-}


{-
class Num - базовый класс для чисел, с которыми выполняем операции

class Num a where
  (+), (*), (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a //переход от любого числа к типу num

  :t fromInteger 3
  - (fromInteger 3) :: Num a => a

(Внутренняя реализация полиморфизма всех чисел)
Все численные констатны в языке определены внутри GHCI как Integer, когда мы их исползуем, они заменяются на вызов функции fromInteger на числе

Реализация по умолчани (-) <=> (negate)
Закон для класса типов Num: // компилятор это не проверяет, это для корректной работы и соблюдения свойств
    abs x * signum x == x

Деление не определено, т к для целых и дробных чисел оно разное.
Два наследника(расширения) Num : Integral and Fractional
Num -> Real -> Integral

  :i Integral
  class (Real a, Enum a) => Integral a where
    quot :: a -> a -> a // на отрицательных числа разное целочисленное деление
    rem :: a -> a -> a // quot rem эффективнее, но по знаку отличается от тех же div and mod
    div :: a -> a -> a // основные
    mod :: a -> a -> a // функции
    quotRem :: a -> a -> (a, a)
    divMod :: a -> a -> (a, a) // (результат, остаток)
    toInteger :: a -> Integer // приводит любой интегральный тип к Integer
          -- Defined in `GHC.Real'
  instance Integral Integer -- Defined in `GHC.Real'
  instance Integral Int -- Defined in `GHC.Real'

  :i Fractional
  class Num a => Fractional a where
    (/) :: a -> a -> a // полноценное деление
    recip :: a -> a
    fromRational :: Rational -> a  // Rational тип рациональных чисел состоит из числителя и знаменателя
          -- Defined in `GHC.Real'
  instance Fractional Float -- Defined in `GHC.Float'
  instance Fractional Double -- Defined in `GHC.Float'

  :i Floating // определены все стандартные мат функции
  class Fractional a => Floating a where
    pi :: a // Pi полиморфна тоже
    exp :: a -> a
    sqrt :: a -> a
    log :: a -> a
    (**) :: a -> a -> a
    logBase :: a -> a -> a
    sin :: a -> a
    tan :: a -> a
    cos :: a -> a
    asin :: a -> a
    atan :: a -> a
    acos :: a -> a
    sinh :: a -> a
    tanh :: a -> a
    cosh :: a -> a
    asinh :: a -> a
    atanh :: a -> a
    acosh :: a -> a
          -- Defined in `GHC.Float'
  instance Floating Float -- Defined in `GHC.Float'
  instance Floating Double -- Defined in `GHC.Float'

  :i RealFrac // содержит набор функций связанных с округлением до целого типа
  class (Real a, Fractional a) => RealFrac a where
    properFraction :: Integral b => a -> (b, a)
    truncate :: Integral b => a -> b // отброс десятичной части
    round :: Integral b => a -> b // округляет
    ceiling :: Integral b => a -> b // верхняя целочисленная грань
    floor :: Integral b => a -> b // нижняя целочисленная грань
          -- Defined in `GHC.Real'
  instance RealFrac Float -- Defined in `GHC.Float'
  instance RealFrac Double -- Defined in `GHC.Float'

  :i RealFloat // описывает внутреннее представление числе с плавающей точкой, внутри компа они реализованны в виде мантиссы экспоненты и основания
  class (RealFrac a, Floating a) => RealFloat a where
    floatRadix :: a -> Integer
    floatDigits :: a -> Int
    floatRange :: a -> (Int, Int)
    decodeFloat :: a -> (Integer, Int)
    encodeFloat :: Integer -> Int -> a
    exponent :: a -> Int
    significand :: a -> a
    scaleFloat :: Int -> a -> a
    isNaN :: a -> Bool
    isInfinite :: a -> Bool
    isDenormalized :: a -> Bool
    isNegativeZero :: a -> Bool
    isIEEE :: a -> Bool
    atan2 :: a -> a -> a
          -- Defined in `GHC.Float'
  instance RealFloat Float -- Defined in `GHC.Float'
  instance RealFloat Double -- Defined in `GHC.Float'
-}

avg :: Int -> Int -> Int -> Double
avg = \x y z ->  (fromRational (toRational x) + fromRational (toRational y) +  fromRational (toRational z))/ 3
