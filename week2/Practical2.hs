module Practical2 where

import Data.Semigroup

-- INDUCTION, INDUCTIVE PROOFS


-- n = 0 1 命题成立
-- 假设 n=k成立，证明n=k+1成立


-- []成立
-- 假设[xs] 成立，证明x:[xs]成立--[1,2,3] = 1:[2,3]


-- Task 1: `sum (replicate n 1)` is just `n`.
-- Show that
-- (replicate 3 1)=[1,1,1]
-- for any natural number n :: Int, the equality
-- sum (replicate n 1) = n
-- holds.

-- replicate 5 'a' = "aaaaa"
-- replicate 6 4 = [4, 4, 4, 4, 4, 4]

{-replicate' n a | n <= 0 = [] -- eq 1
               | otherwise = a:(replicate' (n-1) a) -- eq 2

sum' :: [Int] -> Int
sum' [] = 0 -- eq 3
sum' (x:xs) = x + sum' xs -- eq 4-}

{-sum (replicate 0 1) = sum [] -- eq 1
                    = 0 -- eq 3

sum (replicate k 1) = k -- IH

sum (replicate (k+1) 1) = sum (1:(replicate k 1)) -- eq 2
                        = 1 + sum (replicate k 1) -- eq 4
                        = 1 + k -- IH
                        = k + 1 -- comm-}

-- Task 2: map distributes over append
-- Show that
-- for any function f :: a -> b
-- and any list xs :: [a]
-- and any list ys :: [a]
-- the equality
-- map f (xs ++ ys) = map f xs ++ map f ys
-- holds.

-- [1, 3, 2, 1, 3] => [3, 5, 4] ++ [3, 5]

{-map :: (a -> b) -> [a] -> [b]
map f [] = [] -- eq 1
map f (x:xs) = f x : map f xs -- eq 2

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys -- eq 3
(x:xs) ++ ys = x:(xs++ys) -- eq 4

map f ([] ++ ys) = map f ys -- eq 3
                 = [] ++ map f ys -- eq 3 (reverse)
                 = map f [] ++ map f ys -- eq 1 (reverse)

Assume this holds for some xs
map f (xs ++ ys) = map f xs ++ map f ys -- IH

map f (x:xs ++ ys) = map f (x:xs) ++ map f ys -- WTS

map f ((x:xs) ++ ys) = map f (x:(xs ++ ys)) -- eq 4
                   = f x : map f (xs ++ ys) -- eq 2
                   = f x : (map f xs ++ map f ys) -- IH
                   = (f x : map f xs) ++ map f ys -- eq 4
                   = map f (x:xs) ++ map f ys -- eq 2 (reverse) -}
-- SIMPLE DATA TYPES, PATTERN MATCHING, CLASSES

-- Task 3. Define a data type `RPS` that can represent
-- the three possible gestures in a Rock, Paper, Scissors game.
data RPS = Rock | Paper | Scissors deriving (Eq,Show)


-- Task 4. Implement (instance Show RPS where)
-- directly instead of deriving it.
instance Show RPS where
    show Rock = "Da"
    show Paper = "Repap"
    show Scissors = "Tijeras"

-- Task 5. Define an operation (<!>) :: RPS -> RPS -> RPS
-- which takes two RPS elements, and returns the winner
-- (or the first input on a tie), using pattern matching.
(<!>) :: RPS -> RPS -> RPS
Rock <!> Scissors = Rock
Scissors <!> Rock = Rock
Rock <!> Paper = Paper
Paper <!> Rock = Paper
Scissors <!> Paper = Scissors
Paper <!> Scissors = Scissors
x <!> _ = x


--z <!> p / r /s
-- Task 6. Can (RPS, <!>) form a Monoid structure?
-- No, because no identity

-- Task 7. Can (RPS, <!>) form a Semigroup structure?
-- R <!> (P <!> S) = R <!> S = R

-- (R <!> P) <!> S = P <!> S = S


-- Task 8. We say that an operation <!> is commutative if
-- the equality `x <!> y == y <!> x` holds for all possible
-- inputs x,y. Is <!> commutative?

-- Yes, it is commutative

-- Task 9. Does this mean that `x <!> y <!> z == x <!> z <!> y`
-- also holds for all possible inputs x, y, z?

-- R <!> (P <!> S) != (R <!> S) <!> P
-- R                    P

-- Task 10. Consider the following data type representing
-- a distance in two different possible units.
data Distance
  = Kilometers Double
  | Miles Double
  deriving (Show)
-- Assuming that 1 mile is exactly 1.6km,
-- define an Eq instance for this type,
-- so that two objects of type Distance
-- are equal precisely if they represent
-- the same distance.
-- ==
instance Eq Distance where
    (Kilometers a) == (Kilometers b) = a == b
    (Miles a) == (Miles b) = a == b
    (Kilometers a) == (Miles b) = a == 1.6*b
    x == y = y == x
    --(Miles b) == (Kilometers a) = a == 1.6*b

-- RECURSIVE DATA TYPES

-- Task 11. Define a recursive data structure `FamTree i`
-- representing a family tree, showing the biological
-- ancestors of an individual. Keep in mind that in some
-- cases one or more ancestors might be unknown.
data FamTree i 
    = NoKnownParents i
    | KnownMother i (FamTree i) 
    | KnownFather i (FamTree i) 
    | KnownMotherFather i (FamTree i) (FamTree i)
    deriving (Show, Eq)


carlo :: FamTree String
carlo = NoKnownParents "Carlo"

louis :: FamTree String
louis = KnownFather "Louis" carlo

hortense :: FamTree String
hortense = NoKnownParents "Hortense"

napoleon3 :: FamTree String
napoleon3 = KnownMotherFather "Napoleon3" hortense louis 
-- Task 12. Define a function `numAncestors :: FamTree i -> Int`
-- that determines the total number of known ancestors in a given
-- family tree.
numAncestors :: FamTree i -> Int
numAncestors (NoKnownParents a) = 0
numAncestors (KnownMother a motherTree) = 1 + numAncestors motherTree
numAncestors (KnownFather a fatherTree) = 1 + numAncestors fatherTree
numAncestors (KnownMotherFather a m f) = 2 + numAncestors m + numAncestors f
-- Task 13. Define a function
-- `maternalLine :: FamTree i -> [i]`
-- that traces the kinship of an individual through the
-- known female line (i.e. the returned list should contain
-- the individual, their mother, the mother's mother,
-- the mother's mother's mother, and so on).
maternalLine :: FamTree i -> [i]
maternalLine (NoKnownParents a) = [a]
maternalLine (KnownMother a m) = a:(maternalLine m)
maternalLine (KnownFather a m) = [a]
maternalLine (KnownMotherFather a m f) = a:(maternalLine m)

-- Task 14. Define a similar function for the paternal (male) line.

-- Left as exercise, I can release answer in next practical if need be.
-- Hint: do something very similar to maternalLine

-- Bonus Task: use the ShapeGraphics.hs library (see Exercise 1) to draw the
-- shapes of family trees using `PictureObject`s such as
-- `Circle` and `Path`.
 
-- Will cover when we cover the ShapeGraphics library, 
-- or possibly if we have time next week 
