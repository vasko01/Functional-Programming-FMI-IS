main :: IO()
main = do
    print (myMin 2 7)
    print (isInside 8 5 10)
    print (calcAverage 2.5 3.7)
    print (fibRec 6)
    print (fibIter 6)


myMin :: Integer -> Integer -> Integer
myMin a b = if a < b then a else b

isInside :: Integer -> Integer -> Integer -> Bool
isInside x a b = x >= a && x <= b

calcAverage :: Double -> Double -> Double
calcAverage a b = (a ^ 2 + b ^ 2) / 2

fibRec :: Integer -> Integer
fibRec n = if n <= 1 then 1 else fibRec (n - 1) + fibRec (n - 2)

fibIter :: Integer -> Integer
fibIter n = fibIterHelper 1 1 n
    where
       -- fibIterHelper :: Integer -> Integer -> Integer -> Integer
        fibIterHelper a b n = if n < 2 then b else fibIterHelper b (a + b) (n - 1)
