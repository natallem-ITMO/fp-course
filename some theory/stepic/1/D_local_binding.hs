module D_local_binding where

{-
В хаскеле отступы играют содержательную роль, они задают двумерный синтаксис. Отступы измеряются в символах пробела. Tab = 8 пробелов
Основной принцип
Увелечиние отступа безопасно. Говорит о продолжении текущего объявления, которое началось на предыдущей строке.
Уменьешние отступа может привести к проблемам, если текущий отступ меньше, чем тот отступ, с которого объявление начиналось.

С нувелевого отступа начинаются глобальные объявления
-}
func :: Double ->
                Double ->
    Double
      -> (Double , Double) --глобальное объявление тип функции roots
func a b c = -- глобальное объявление тела функции
  (
    (a+b)
  ,
    (b+c)
  )


{-Связывание помогает избегать дублирование в функции.
(d = b ^ 2 - 4 * a * c) - локальное связывание
(x1 = (-b + sqrt d) / twiceA) - некоторое выражение связывается с некоторой переменной, которая будет использоваться в блоке instance  where
Порядок связывания не важен, можно делать даже рекурсию.
В хаскеле многие объявления заключаются в фигурные скобки и разделяются ; (если нужно уложить в одну строку в ghci например)
-}

roots' :: Double -> Double -> Double -> (Double , Double)
roots' a b c =
  let {x1 = (-b + sqrt d) / twiceA;x2 = (-b - sqrt d) / twiceA; d = b ^ 2 - 4 * a * c;twiceA = 2 * a}
  in (x1, x2)

roots :: Double -> Double -> Double -> (Double , Double)
roots a b c =
  let
    x1 = (-b + sqrt d) / twiceA--локально определенная функция
    x2 = (-b - sqrt d) / twiceA--важно, что отступы для локального связывания одинаковые, иначе если отступ больше, то считаем как продолжение объявления предыдущего свяязывания
    d = b ^ 2 - 4 * a * c
    twiceA = 2 * a
  in (x1, x2)


{-
C помощью let in можно
*давать локальные имена подвыражениям
*определять локальные функции (которые засоряли бы глобальное пространство имен)
-}
helper acc x = 342048
factorial n | n < 0 = error "arg should be >= 0"
            | otherwise =
              let
                helper acc x
                        | x == 0 = acc
                        | otherwise = helper (acc * x) (x-1)
              in helper 1 n

difRoots a b c =
  let
    (x1, x2) = roots a b c
  in x1-x2
{-
В let in можно связать
  * локальные переменные
  * определение функции
  * образец
 -}


{-         Where - еще одна конструкция, обеспечивающая локальное связывание(но сначала идет выражение, в котором используются какие то переменные, а потом блок, где локальное свяязвание)



-}
roots'' :: Double -> Double -> Double -> (Double , Double)
roots'' a b c = (x1, x2) where
    x1 = (-b + sqrt d) / twiceA--локально определенная функция
    x2 = (-b - sqrt d) / twiceA--важно, что отступы для локального связывания одинаковые, иначе если отступ больше, то считаем как продолжение объявления предыдущего свяязывания
    d = b ^ 2 - 4 * a * c
    twiceA = 2 * a
{- Синтаксис разделения отдельных локальных связываний такой же, как в let in {;} или список

Отличие:
let in - конструкция - выражение
  let x = 2 in x^2
  - 4
  (let x = 2 in x^2)^2
  - 16
where - не выражение. Эта конструкция используется только в определении фукнции на опр. месте в кач-ве глобальной части тела этой функции.
Но: where можно использовать сразу в нескольких уравнениях с охранными выражениями(при разделении огранными выражениями тело функции перестает быть одним выражением и let in в нем использовать не получится)
-}

sum'n'count :: Integer -> (Integer, Integer)--sum count
sum'n'count x | x == 0 = (0,1)
              | x > 0 = helper x 0 0
              | otherwise = helper (-x) 0 0 where
  helper x sum count | x == 0 = (sum, count)
                     | otherwise = let
                          (quotient, remainder) = quotRem x 10
                       in helper quotient (sum + remainder) (count + 1)



