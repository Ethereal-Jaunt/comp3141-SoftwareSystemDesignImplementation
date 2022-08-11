module Lecture6 where

-- DEMO 1

returnL :: a -> [a]
returnL x = [x]

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
bindL :: [a] -> (a -> [b]) -> [b]
bindL xs f = concat (map f xs)


-- f x = [x+1]
-- xs =[1,2,3]
-- (map f xs) = [[2],[3],[4]]
-- [[] ,[], []]



-- let ne x = [x-1,x+1]
-- ne 5 = [4,6]
-- bindL [4,9,13] ne 
-- bindL [4,9,13] $\x -> ne x


-- Nothing >>= f = Nothing
-- Just a  >>= f = Just (f a)

-- DEMO 2
-- [1,2,3]
{-
roll two six-sided dice, add their results, then add another 3
-}

d6 :: [Int]
d6 = [1,2,3,4,5,6]

-- raw bind notation
two_d6 :: [Int]
two_d6 =
  d6 >>= \x ->
  d6 >>= \y ->
  return (x + y + 3)

-- do notation
two_d6' :: [Int]
two_d6' = do
  x <- d6
  y <- d6
  return (x + y + 3)

-- list comprehension notation
two_d6'' :: [Int]
two_d6'' =
  [ x + y + 3 | x <- d6, y <- d6 ,x<5]


-- DEMO 3

nums :: [Int]
nums = [6,21,15,3,10]

type Solution = [Int]

extend :: Solution -> [Int] -> [Solution]
extend [] xs = map (\x -> [x]) xs
extend (s:sol) xs =
    map (\x -> x:s:sol) $
    filter (\x -> gcd x s > 1) $ xs

solve :: Solution -> [Int] -> [Solution]
solve sol [] = return sol
solve sol xs = do
  sol' <- extend sol xs --得到拓展后的solution
  let xs' = filter (not . (`elem` sol')) xs --筛选出还没有用过的值
  solve sol' xs'  --递归调用


-- DEMO 4

maybeMap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeMap2 f Nothing _ = Nothing
maybeMap2 f _ Nothing = Nothing
maybeMap2 f (Just x) (Just y) = Just (f x y)

idea :: Maybe (a -> b) -> Maybe a -> Maybe b
idea Nothing _ = Nothing
idea (Just f) Nothing = Nothing
idea (Just f) (Just x) = Just (f x)

