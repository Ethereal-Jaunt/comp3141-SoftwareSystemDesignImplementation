module Example where

isOdd :: Int -> Bool
isOdd x = isEven (x + 1)

isEven :: Int -> Bool
isEven x = ((x `mod` 2) == 0)

isPositive :: Int -> Bool
isPositive y = y > 0

isNonnegative :: Int -> Bool
isNonnegative x = (x == 0) || isPositive x

log10 :: Double -> Double
log10 = logBase 10

log2 :: Double -> Double
log2 = logBase 2

toNthPower :: Double -> (Double -> Double)
toNthPower n x = x ** n

cube :: Double -> Double
cube x = toNthPower 3 x

-- golden ratio
phi :: Double
phi = (1 + sqrt 5) / 2

neighbors :: Int -> (Int, Int)
neighbors x = (x - 1, x + 1)

div2 :: Int -> (Int, Int)
div2 x = (divResult, remainder) where
  divResult :: Int
  divResult = x `div` 2
  remainder :: Int
  remainder = x `mod` 2

check2 :: (Int, Int) -> Int
--check2 (d,r) = 2*d + r  -- <-- pattern matching
check2 x = 2*(fst x) + (snd x)


applyTwice :: (t -> t) -> t -> t
applyTwice f x = f (f x)
--         ^ ^
--         | |
--         | x :: t
--         f :: (t -> t)

square :: Int -> Int
square x = x * x

fourthPower :: Int -> Int
fourthPower = applyTwice square

{- EQUATIONAL REASONING

   fourthPower 3
== applyTwice square 3
== square (square 3)
== (square 3) * (square 3)
== (3 * 3) * (3 * 3)
== 9 * 9
== square 9
== 81

-}
