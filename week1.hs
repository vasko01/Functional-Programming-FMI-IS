main :: IO()
main = do
    print (myMin 2 7)
    print (isInside 8 5 10)
    print (calcAverage 2.5 3.7)
    print (fibRec 6)
    print (fibIter 6)

{-
Задача 1. Да се дефинира функция myMin, която приема два аргумента и връща по-малкия от тях.
-}
myMin :: Integer -> Integer -> Integer
myMin a b = if a < b then a else b

{-
Задача 2. Да се дефинира функцията isInside x a b, която проверява дали числото x се намира в затворения интервал [a, b].
-}
isInside :: Integer -> Integer -> Integer -> Bool
isInside x a b = x >= a && x <= b

{-
Задача 3. Да се дефинира функция calcAverage, която пресмята на средно аритметичното от квадратите на 2 числа.
-}
calcAverage :: Double -> Double -> Double
calcAverage a b = (a ^ 2 + b ^ 2) / 2

{-
Задача 4. Да се дефинира fibRec, която получава един аргумент n и връща n-тото число на Фибоначи, чрез рекурсивен процес. 
-}
fibRec :: Integer -> Integer
fibRec n = if n <= 1 then 1 else fibRec (n - 1) + fibRec (n - 2)

{-
Задача 5. Да се дефинира fibIter, която получава един аргумент n и връща n-тото число на Фибоначи, чрез итеративен процес.
-}
fibIter :: Integer -> Integer
fibIter n = fibIterHelper 1 1 n
    where
       -- fibIterHelper :: Integer -> Integer -> Integer -> Integer
        fibIterHelper a b n = if n < 2 then b else fibIterHelper b (a + b) (n - 1)
