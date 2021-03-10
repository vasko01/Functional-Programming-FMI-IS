main::IO()
main = do
    print (pow 2 10)
    print (isPrime 83)
    print (isAscending 149)
    print (countOccurences 455835 5)
    print (isPerfectNumber 28)
    print (sumPrimeDivisors 15) 


{-
Задача 1. Да се дефинира функция pow, която генерира 
линейно рекурсивен процес и намира x на степен n, където x е реално,
а n - естествено число.
-}
pow :: Double -> Integer -> Double
pow  x n
    | n == 0    = 1
    | otherwise = x * pow x (n-1)

pow' :: Double -> Int -> Double
pow' x n = helper n 1
    where
        helper n result =
            if n == 0 then result else helper (n - 1) (result * x)

{-
Задача 2. Да се дефинира предикат isPrime, който проверява дали дадено естествено число е просто.
-}
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = helper 2
    where   
        helper d
            | d == n       = True
            | mod n d == 0 = False
            | otherwise    = helper (d + 1)

{-
Задача 3. Да се дефинира предикат isAscending, който връща истина,
 ако цифрите на дадено естествено число са в нарастващ ред от първата към последната.
-}
isAscending :: Integer -> Bool
isAscending n
    | n < 10 = True
    | otherwise = helper (div n 10) (mod n 10)
    where    
        helper num lastDigit      
            | num < 10                  = num < lastDigit
            | (mod num 10) >= lastDigit = False
            | otherwise                 = helper (div num 10) (mod num 10)   

{-
Задача 4. Да се дефинира функция countOccurences, намираща броя на срещанията на дадена цифра d в записа на число n.
-}
countOccurences :: Integer -> Integer -> Integer
countOccurences n d
    | n < 10 = if n == d then 1 else 0
    | otherwise = helper n 0
        where
            helper k count
                | k == 0          = count
                | k `mod` 10 == d = helper (k `div` 10) (count + 1)
                | otherwise       = helper (k `div` 10) count

{-
Задача 5. Да се дефинира предикат isPerfectNumber, който връща дали едно число е съвършено,
т.е. равно на сумата от делителите му.
-}

sumDel :: Integer -> Integer
sumDel n = helper 1 0
    where 
        helper del sum
            | del == n         = sum
            | n `mod` del == 0 = helper (del + 1) (sum + del)
            | otherwise        = helper (del + 1) sum

isPerfectNumber :: Integer -> Bool
isPerfectNumber n = n == sumDel n

{-
Задача 6. Да се дефинира функция sumPrimeDivisors, която намира сумата на всички прости делители на едно число.
-}

sumPrimeDivisors :: Integer -> Integer
sumPrimeDivisors n = helper 1
    where
        helper d
            | n == d                      = 0
            | n `mod` d == 0 && isPrime d = d + helper (d + 1)
            | otherwise                   = helper (d + 1)