main :: IO()
main = do
    print (myGCD 25 5)
    print (myGCD' 25 5)
    print (25 `myGCD''` 5)
    print (countDigitsRec 2564)
    print (sumDigitsRec 5)
    print (countDigitsIter 2648)
    print (sumDigitsIter 1234)
    print (reverseNumberRec 456789)
    print (reverseNumberIter 123456)
    print (countPalindromes 100 1000)
    print (countPalindromesIter 100 1000)

{-
Задача 1. Да се дефинира функция myGCD a b, която връща най-големия общ делител на числата a и b.
-}
myGCD :: Integer -> Integer -> Integer
myGCD a b = if b == 0 then a else myGCD b (a `mod` b)

myGCD' :: Integer -> Integer -> Integer
myGCD' a b = 
    if a == b then a
    else if a > b then myGCD' (a - b) b
    else myGCD' a (b - a)

myGCD'' :: Integer -> Integer -> Integer
myGCD'' a b 
    | a == b    = a
    | a > b     = myGCD'' (a - b) b
    | otherwise = myGCD'' a (b - a)

{-
Задача 2. Да се дефинира функция countDigits, която генерира линейно рекурсивен процес и намира броя на цифрите на дадено естествено число.
-}
countDigitsRec :: Integer -> Integer
countDigitsRec n = if n < 10 then 1 else 1 + countDigitsRec (n `div` 10)

countDigitsIter :: Integer -> Integer
countDigitsIter n = helper n 1
    where
        helper n res = if n < 10 then res else helper (n `div` 10) (res + 1)      

{-
Задача 3. Да се дефинира функция sumDigitsRec, която генерира линейно рекурсивен процес и намира сумата от цифрите на дадено естествено число.
-}
sumDigitsRec :: Integer -> Integer
sumDigitsRec n = if n == 0 then 0 else n `mod` 10 + sumDigitsRec (n `div` 10)

{-
Задача 4. Да се дефинира функция sumDigitsIter, която генерира линейно итеративен процес и намира сумата от цифрите на дадено естествено число.
-}
sumDigitsIter :: Integer -> Integer
sumDigitsIter n = helper n 0
    where
        helper n res = 
            if n == 0 then res
            else helper (n `div` 10) (res + n `mod` 10)

{-
Задача 5. Да се дефинира функция reverseNumber, която генерира линейно итеративен процес и по дадено естествено число n намира числото,
 записано със същите цифри, но в обратен ред.
-}
reverseNumberRec :: Integer -> Integer
reverseNumberRec n = if n < 10 then n else (n `mod` 10) * 10 ^ (countDigitsRec n - 1) + reverseNumberRec (n `div` 10)

reverseNumberIter :: Integer -> Integer
reverseNumberIter n = helper n 0
    where
        helper num res
            | num == 0 = res
            | otherwise = helper (num `div` 10) (res * 10 + num `mod` 10)

{-
Допълнителна задача. Дефинирайте двуаргументна функция countPalindromes a b, която намира броя на
числата палиндромите в затворения интервал [a, b].
-}
isPalindrome :: Integer -> Bool
isPalindrome n = n == reverseNumberIter n

countPalindromes :: Integer -> Integer -> Integer
countPalindromes a b 
    | a > b          = 0
    | isPalindrome a = 1 + countPalindromes (a + 1) b
    | otherwise      = countPalindromes (a + 1) b
               

countPalindromesIter :: Integer -> Integer -> Integer
countPalindromesIter a b = helper a b 0
    where
        helper a b res
            | a > b                = res
            | not (isPalindrome a) = helper (a + 1) b res
            | isPalindrome a       = helper (a + 1) b (res + 1)