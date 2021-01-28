module C where
{-Специальный полиморфизм
Фукнция может вызываться на разных типах данных, но каждый тип данных спец. полиморфизм должен реализовывать интерфейс.
Класс типов описывает такой интерфейс целиком.
Реализация класса типов - реализация представителя.
Тип данных должен объявлять представителя класса типов(т.е. имплементировать соответвующий интерфейс), после имплементации можем в специальную полиморфную фукнцию такой тип данных передавать.
Интерфейсы классов типов именнованы.

  :t 7
  -7 :: Num a => a
Знак следования в типе разделяет его на 2 части. Правая часть - тип выражения. Слева - контекст.
Контекст состоит из 2 частей
1) Имя интерфейса для типа из правой части (
2) Интерфейс применен к типу

1) 7 имеет полиморфный тип а
2) Для типа а должен быть выстроен интерфейс Num

Num выстравляет ряд функций. Числовой класс типов. Фукнции + * -
  :t (+)
  -(+) :: Num a => a -> a -> a // (в контексте наложено ограничение, а относится к Num)

  :t (>)
  -(>) :: Ord a => a -> a ->  Bool

  :t (> 7)
  -(> 7) :: (Num a, Ord a) => a -> Bool // Num Ord разные контексты, например комплекные типа Num но не  Оrd

Пары сравниваются лексикографически. Первый элементы могут быть даже разных числовых типов (Int and Double)

  :t (> (1,2))
  -(> (1,2)) :: (Num t, Num t1, Ord t, Ord t1) => (t, t1) -> Bool

  :t (* 'c') // сечение оператора множение строковой константой 'c'
  - No instance for (Num Char) arising from a use of `*'
    In the expression: (* 'c')
-}


{-
Класс типов задает интерфейс, которые типы могут реализовывать.
Сам по себе класс типов представляет собой именнованый набор имен функций с сигнатурами, параметризованный общим типовым параметром.
-}
{-
class Eq a where
  (==) :: a -> a -> Bool --  a -> a -> Bool - сигнатура
  (/=) :: a -> a -> Bool

Сигнатуры функции должны начинаться с НЕНУЛЕВОГО ОТСТУПА. Т.к. это не глобальное объявление

  :t (==)
  - (==) :: Eq a => a -> a -> Bool (сигнатура отличается только наличием контекста)

  :t (== 43)
  - (== 43) :: (Eq a, Num a) => a -> Bool

  :t (== 'x')
  - (== 'x') :: Char a => a -> Bool (Char реализует класс типов Eq, не обозначается дважды)

  :t elem // функция проверки если ли в списке элемент
  -elem :: Eq a => a -> [a] -> Bool //должны сравнивать элементы
-}


{-
Конкретный тип - представитель (экземпляр)(instance) класса типов, если для него реализованы все функции из класса типов.

class Eq a where
  (==), (/=) :: a -> a -> Bool

instance Eq Bool where
  True == True = True
  False == False = True
  _     == _     = False // _ - образец всегда удачен
  x /= y = not (x == y)

Отличие интерфесов в ООП и хаскеле:
    В ООП мы реализуем интерфейс для некоторого типа данных там же, где мы определяем тип.
    В хаскеле можем делать любой тип представителем класса в отдельном месте от определения класса типов и определения самого типа.

Допускается реализация по умолчанию в объявлении класса типов:

class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)

instance Eq Bool where
  True == True = True
  False == False = True
  _     == _     = False // _ - образец всегда удачен

В таком случае от типов требуется реализовать только фукнции без реализации по умолчанию
Но можно и перекрывать реализацию по умолчанию.

Можно давать циклические реализации по умолчанию

class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)

Minimal complete definition - в документации есть такой термин минимального полного определения (говорит, что необходимо реализовать)

Может быть такое что для всех методов есть реализация по умолчанию. Тогда

instance Complete Bool where

Но надо быть внимательным к циклической реализации
-}


{-
Представителем класса типов можно объвлять и некоторый полиморфный тип

Например есть картеж из 2 элементов. Но в Eq должны проверять соответсвие типов

instance (Eq a, Eq b) => Eq (a,b) where // появляется контекст, отражающий ограничения
  pq == pq2 = fst p1 == fst p2 && snd p1 == snd p2

Приоритет (==) 4, (&&) 3.

В общем случае фукнции сравнивать на равенство невозможно(типы стрелочного типа не экземпляры Eq).
-}