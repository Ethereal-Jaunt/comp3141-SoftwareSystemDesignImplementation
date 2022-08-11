module Practical3 where

import Data.Semigroup
import Data.List
import Test.QuickCheck

-- FUN WITH FIBONACCI --

-- Recall that the Fibonacci numbers are defined
-- by the following recurrence relations:
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)
-- Notice that while `fibo 10` can easily be evaluated,
-- as can `fibo 20`, evaluating `fibo 40` directly
-- from this definition is way too slow.
--10 9 8 8 7

-- 0 1 2 3 ... 10
-- Task 1. Give a faster implementation `fib :: Integer -> Integer`.
fib :: Integer -> Integer
fib x = fib' 0 1 0 where
--             a b 
    fib' a b i
      | i < x     = fib' b (a+b) (i+1)
      | otherwise = a

-- x=3  a b i
-- fib' 0 1 0  --1
-- fib' 1 1 1  --2
-- fib' 1 2 2  --3
-- fib' 2 3 3  --4
-- 2


-- Task 2. Write a QuickCheck specification for the functional
-- correctness of `fib`.
prop_fibCompare :: Positive Integer -> Bool
prop_fibCompare (Positive x) = fib x == fibo x

prop_fibCorrect :: Positive Integer -> Property
prop_fibCorrect (Positive n) = (n < 10000) ==> n == 0 || n == 1 || fib n == fib (n-1) + fib (n-2)
-- ENTER THE MATRIX --

-- The following data type `M22` defines 2x2 integer matrices.
-- The function `mmul :: M22 -> M22 -> M22` implements
-- matrix multiplication for such matrices.

-- E.g. the value `M22 1 0 2 3` denotes the matrix usually
-- written as
-- [1 0]
-- [2 3].

data M22 = M22 Integer Integer Integer Integer deriving (Show, Eq)

mmul :: M22 -> M22 -> M22
mmul (M22 a00 a01 a10 a11) (M22 b00 b01 b10 b11) =
  M22 c00 c01 c10 c11 where
    c00 = a00*b00 + a01*b10; c01 = a00*b01 + a01*b11
    c10 = a10*b00 + a11*b10; c11 = a10*b01 + a11*b11

-- Task 3. Declare Semigroup and Monoid instances
-- for M22. What is the identity element?
instance Semigroup M22 where
    (<>) = mmul


instance Monoid M22 where
    mappend = mmul
    mempty = M22 1 0 0 1

-- Task 4. Use QuickCheck to verify that the matrix
-- multiplication operation forms a Monoid, with
-- identity element M22 1 0 0 1.
--交换律：
prop_assoc22 :: M22 -> M22 -> M22 -> Bool
prop_assoc22 x y z = (x <> y) <> z == x <> (y <> z)
--单位元：
prop_rid :: M22 -> Bool
prop_rid x = x <> mempty == x

prop_lid :: M22 -> Bool
prop_lid x = mempty <> x == x

prop_id :: M22 -> Bool 
prop_id x = (prop_lid x) && (prop_rid x)
{- Let I = Integer
I -> I -> I -> I -> M22 -- Type of M22 constructor
Gen I -> Gen (I -> I -> I -> M22) -- <$>
Gen I -> Gen I -> Gen (I -> I -> M22) -- <*>
Gen I -> Gen I -> Gen I -> Gen (I -> M22) -- <*>
Gen I -> Gen I -> Gen I -> Gen I -> Gen M22 -- <*>-}
-- 类型类（类型的集合）
-- Arbitrary能生成随机值的数据类型
instance Arbitrary M22 where
    arbitrary = M22 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


-- Task 5. Consider the matrix `fim = M22 1 1 1 0`.
-- This is sometimes called the Fibonacci matrix.
-- Show, using QuickCheck, that the second coefficient
-- of the matrix power fim^n is always the nth Fibonacci
-- number.
-- [1 1]
-- [1 0]
fim :: M22
fim = M22 1 1 1 0

--expo fim n == fim <> fim <> fim <> fim <> ... <> fim -- n times
--即证明expo fim n结果中的b为斐波那契数列的第n项
prop_expo_fim :: Positive Integer -> Property
prop_expo_fim (Positive n) = (n < 10000) ==> case expo fim n of
                                                   M22 _ _ b _ -> b == fib n

-- Task 6. Use the fast monoid exponentiation algorithm `fexpo`
-- (given below, covered in detail in Lecture 3) to
-- implement an even faster Fibonacci function `ffib`.
-- Use it to compute `fib 1000000`.

fexpo :: (Monoid g) => g -> Integer -> g
fexpo x 0 = mempty
fexpo x n
  | even n = let y = fexpo x (n `div` 2) in mappend y y
  | otherwise = mappend x (fexpo x (n - 1))

expo x n = mconcat (replicate (fromIntegral n) x)
-- mconcat :: Monoid a => [a] -> a
-- mconcat ["Hello", " ", "Haskell", "!"]
-- "Hello Haskell!"
-- expo fim 3
-- M22 3 2 2 1
-- fromIntegral 4
-- 4
-- replicate 4 True
-- [True,True,True,True]

--矩阵快速求斐波那契数列的第n项
ffib :: Integer -> Integer
ffib n = let M22 _ b _ _ = fexpo fim n
         in b

-- SORTING THINGS OUT --

-- Task 7. The following four predicates describe
-- four properties that any sorting function should meet.
-- Your job is to show that these properties do not
-- fully specify the `sort` function, by exhibiting
-- two different functions `f :: [Int] -> Int`
-- that satisfy all four properties.

-- Recall that two functions are different if they have
-- different output for at least one input.

--Preserves length 
--排序完长度不变
sortProp1 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp1 sortFn xs =  length xs == length (sortFn xs) 

-- Same on reverse 
-- 反转后排序相同
sortProp2 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp2 sortFn xs = sortFn xs == sortFn (reverse xs)

-- The list is sorted
-- 排序有效
sortProp3 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp3 sortFn xs = isSorted (sortFn xs)
  where
    isSorted (x1 : x2 : xs) = (x1 <= x2) && isSorted (x2 : xs)
    isSorted _ = True

-- Every element in original list is in the sorted list
-- 元素不变
sortProp4 :: ([Int] -> [Int]) -> Int -> [Int] -> [Int] -> Bool
sortProp4 sortFn x xs ys = x `elem` sortFn (xs ++ [x] ++ ys)

dsort :: [Int] -> [Int]
dsort xs = dup_first nodupes (len - length nodupes) where
    len = length xs
    nodupes = sort $ nub xs
-- nodepes  无重复元素的排好序的列表
-- nub去除重复元素
-- dup_first复制n个第一个元素
    dup_first [] _     = [] 
    dup_first xs 0     = xs
    dup_first (x:xs) n = dup_first (x:x:xs) (n-1)

-- [1,2,3,5,1,2,7]
-- [1,2,3,4,7]
-- [1,1,1,2,3,4,7]

sortPropReal :: [Int] -> Bool
sortPropReal xs = sort xs == dsort xs
--Failed!

dsort' :: [Int] -> [Int]
dsort' [] = []
dsort' xs = if take 2 xs' == [0, 0] then [-1, 0] ++ drop 2 xs' else xs' where xs' = sort xs
--如果排完序的列表前两项是0，则把前两项换成-1，0
sortPropReal' :: [Int] -> Bool
sortPropReal' xs = sort xs == dsort' xs
-- quickCheck可以过但实际不成立
-- quickCheck sortPropReal'
-- +++ OK, passed 100 tests.

-- sortPropReal' [2,43,5,7,8,5,8,0,3,0]
-- False


-- Task 8. (Bonus): Try to write functions that satisfy only
-- some of the four properties, but not others. E.g. 1,4 but not 2,3.


{-
1, 4 but not 2, 3 => f xs = xs

3, 4 but not 1, 2 => f (x:xs) = sort x:x:xs

2, 3, 4 but not 1 => f xs = sort $ nub xs

1, 2, 3, not 4 => f xs = [0..(length xs - 1)]

1, 3, 4 not 2 => f (x:xs) = let xs' = (x:xs) without first duplicate. 
                            in if xs' == (x:xs) sort x:xs otherwise sort x:xs'
-}