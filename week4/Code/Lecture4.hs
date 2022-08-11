module Lecture4 where

import Data.Semigroup
import Data.List
import Test.QuickCheck

import Email

-- Round-tripping is a very powerful testing technique:
-- the only time it doesn't catch a bug is if you made a
-- mistake in the implementation of your function, and
-- somehow managed to make the "opposite" mistake in the
-- implementation of its inverse. This is very unlikely.
-- 
-- Cf. Implementing the same function in two different ways:
-- if you do that, you're likely to miss the same edge cases
-- in both implementations. This is much less likely when you
-- implement inverses.
--
-- Disadvantages: Implementing the inverse
-- takes time, and is sometimes more difficult than implementing
-- the original function.

data Example
  = Con1 Int
  | Con2 Example Example

showExample :: Example -> String
showExample (Con1 x) =
  "Con1 " ++ show x
showExample (Con2 x y) =
  "Con2 (" ++ showExample x ++ ") (" ++ showExample y ++ ")"

readExample :: String -> Maybe Example
readExample _ = Nothing
-- try writing this at home, it's much harder than writing show


-- Idempotence

-- sort should be idempotent, sort (sort xs) == xs;
-- but this implementation is dreadfully wrong
-- despite being idempotent
sort' :: (Ord a) => [a] -> [a]
sort' xs = []

prop_sort :: [Int] -> Bool
prop_sort xs = sort' (sort' xs) == sort' xs
-- moral: idempotence by itself is rarely enough,
--        but it can be useful as part of a larger test suite.
--        The larger the test suite, the more potential bugs
--        it can detect.


-- Easy to test, hard to compute.

-- Often it's easier to test that you have the right answer
-- than to compute the answer in the first place.

-- Write a test which asks the question:
-- is xs the right answer to the problem of getting a prime
-- factorization of x?
test_factorize :: Int -> [Int] -> Bool
test_factorize x xs = product xs == x

-- Implement the function somehow (note: this is _intentionally_
-- not a correct implementation).
factorize_impl :: Int -> [Int]
factorize_impl 0 = [0]
factorize_impl x = filter (`divides` x) primes where
  divides :: Int -> Int -> Bool
  x `divides` y = y `mod` x == 0
  primes = 2:[x | x <- [3..1000], all (\y -> not (y `divides` x)) [2..x-1]]
  

-- Turn this into a property test:
-- is the thing that the implementation outputs the right
-- answer to the problem of factorizing x?
prop_factorize :: (Positive Int) -> Bool
prop_factorize (Positive x) = test_factorize x (factorize_impl x)
