{-# LANGUAGE DataKinds, KindSignatures, GADTs, StandaloneDeriving #-}

module Lecture9 where

-- DEMO: Simple ADTs

data Parity :: * where
  Even :: Parity
  Odd  :: Parity
deriving instance Show Parity
deriving instance Eq Parity

nextParity :: Parity -> Parity
nextParity Even = Odd
nextParity Odd  = Even

parity :: Int -> Parity
parity 0 = Even
parity n = nextParity $ parity (n-1)

-- 3 
-- nextParity $ parity 2
-- nextParity $ nextParity $ parity 1
-- nextParity $ nextParity $ nextParity $ parity 0
-- nextParity $ nextParity $ nextParity $ Even

data Polarity :: * where
  Positive :: Polarity
  Zero     :: Polarity
  Negative :: Polarity
deriving instance Show Polarity
deriving instance Eq Polarity
-- data Priority = Positive | Zero | Negative

sign :: Int -> Polarity
sign x
  | x > 0 = Positive
  | x < 0 = Negative
  | otherwise = Zero

-- DEMO: Slightly more complicated ADTs, Sum/Prod types

data Sum :: * -> * -> * where
  L :: a -> Sum a b
  R :: b -> Sum a b
deriving instance (Show a, Show b) => Show (Sum a b)
deriving instance (Eq a, Eq b) => Eq (Sum a b)

-- data Sum a b = L a | R b deriving(Show,Eq)
-- Sum Parity Polarity
-- L Parity : L Even , L Odd
-- R Polarity : R Positive,...




divErr :: Int -> Int -> Sum String Int
divErr x 0 =
  L $ "Division by zero in divErr " ++ show x ++ " " ++ show 0
divErr x y = R (x `div` y)

allElementsOfSumParityPolarity :: [Sum Parity Polarity]
allElementsOfSumParityPolarity =
  [ L Even, L Odd
  , R Positive, R Negative, R Zero
  ]


data Prod :: * -> * -> * where
  Pair :: a -> b -> Prod a b
deriving instance (Show a, Show b) => Show (Prod a b)
deriving instance (Eq a, Eq b) => Eq (Prod a b)

-- Prod Parity Polarity 类型 Int Bool
-- Pair Parity Polarity
-- Pair Even Positive :: Prod Parity Polarity
-- Pair Even undefined
--  值                      类型
-- 3 :: Int

-- data Prod a b = Pair a b

allElementsOfProdParityPolarity :: [Prod Parity Polarity]
allElementsOfProdParityPolarity =
  [ Pair Even Positive
  , Pair Even Negative
  , Pair Even Zero
  , Pair Odd  Positive
  , Pair Odd  Negative
  , Pair Odd  Zero
  ]

-- Demo: Implementing Vec

data Size = Z | S Size deriving(Show,Eq)
-- S $ S $ S $ S Z

data Vec :: * -> Size -> * where
  Nil  :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)
deriving instance (Show a) => Show (Vec a n)
deriving instance (Eq a) => Eq (Vec a n)

--Cons 3 (Cons 2 Nil)  :: Vec a (S n)
--             a Vec a Z
--     ^ ^^^^^^^^^^^
--     a   Vec a n

toList :: Vec a n -> [a]
toList Nil = []
toList (Cons x xs) = x:toList xs

-- Vec a Z     -- definitely empty
-- Vec a n     -- possibly empty
-- Vec a (S n) -- definitely not empty

safeHead :: Vec a (S n) -> a
safeHead (Cons x xs) = x

mapV :: (a -> b) -> Vec a n -> Vec b n
mapV f Nil = Nil
mapV f (Cons x xs) = Cons (f x) (mapV f xs)

zipV :: Vec a n -> Vec b n -> Vec (a,b) n
zipV Nil Nil                 = Nil
zipV (Cons x xs) (Cons y ys) = Cons (x,y) (zipV xs ys)

-- [2,3,+,5,6,-]
-- [2,+] -> Nothing
-- [0,2,+]

-- Demo: RPN Calculator

-- Commands: Enter (-), Arith (+), AllClear
-- we used to give lists of commands
-- [Enter 3, Enter 2, Arith (+)]

-- RPN is a data type that stores lists of RPN commands
-- stack size: before  after
--             v       v
data RPN :: Size -> Size -> * where
  End      :: RPN n n
  Enter    :: Double -> RPN (S n) m -> RPN n m
  Add      :: RPN (S n) m -> RPN (S (S n)) m
  AllClear :: RPN Z m -> RPN n m

-- data RPN n m = End 
--               | Enter Double (RPN (S n) m)
--               | Add RPN (S n) m
--               | AllClear RPN Z m 

eval :: RPN n m -> Vec Double n -> Vec Double m
eval End            xs = xs
eval (Enter n rest) xs = eval rest (Cons n xs)
eval (Add rest)     (Cons x (Cons y xs)) =
   eval rest (Cons (y+x) xs)
eval (AllClear rest) xs = eval rest Nil

-- eval (Enter 2 $ Enter 3 $ Add $ End ) Nil
-- -- eval (Enter 2 $ Enter 3 $ End ) Nil


{- Equational Reasoning I

Recall that we had to prove that the map function
`map :: (a -> b) -> [a] -> [b]`
preserves the length of its argument, i.e. given any fixed `f :: a -> b`
we have that for all lists `xs :: [a]`, the equality
`length (map f xs) == length xs`
holds.

Make sure you are comfortable with this proof: you'll be expected
to do similar proofs in the future.

For equational reasoning, we first write down the relevant equations.
In this statement, two functions appear, `length` and `map`, so the
relevant equations will be the ones that define these two:

length [] = 0
length (x:xs) = 1 + length xs
map f [] = []
map f (x:xs) = f x : map f xs

Once these are written down, we can start our proof by
structural induction.

We want to show that for all lists `xs :: [a]`, the equality
`length (map f xs) == length xs` holds.

We have two cases to consider:

1. Base case when xs is [].
   Goal: length (map f []) == length []

   Argument:
   length (map f []) ==         // by `map f [] == []`, 1st eqn of map
   length [].

2. Inductive case when xs is (y:ys)
   Inductive hypothesis: length (map f ys) = length ys.
   Goal:                 length (map f (y:ys)) = length (y:ys).

   Argument:
   length (map f (y:ys))   ==   // by 2nd eqn of map
   length (f y : map f ys) ==   // by 2nd eqn of length
   1 + length (map f ys)   ==   // by inductive hypothesis
   1 + length ys           ==   // by 2nd eqn of length (backwards)
   length (y:ys).

Thus, by the principle of structural induction, we have shown that
`length (map f xs) == length xs` holds for all lists `xs`.

With `mapV`, we don't need to prove this manually: it follows from
the type signature that `mapV` cannot change the length of its input.

-}



{- Equational Reasoning II

Let's finish up the course with some equational reasoning
and structural induction, this time on the structure of the
GADT called RPN.

Instead of eval, defined above, we could have defined
an unsafe version, `ueval`, which uses unsized lists
instead of sized vectors.

ueval :: RPN -> [Double] -> [Double]
ueval End xs = xs
ueval (Enter n rest) xs = ueval rest (n:xs)
ueval (Add rest) (x:y:xs) = ueval rest (y+x:xs)
ueval (AllClear rest) xs = ueval rest []
ueval _ _ = error "Unhandled case"

We are going to show that, although `ueval` has unhandled
cases, while `eval` is fully defined, in the cases where
both are defined, the two programs always have the same
result. More precisely: converting the stack to a list
and then using ueval yields the same result as first
using eval, then converting the resulting stack to a list.

The induction underlying this proof is much more sophisticated
than the previous proofs by induction that we've encountered.
You don't have to be able to produce proofs this complex; but
you can use it as a measuring stick. If you understand the
logic of this proof, you're more than ready to tackle any of the
proofs that might come up in this course.

Formally, we are going to show that:
For all `p :: RPN n m`, and for all stacks `xs :: Vector Double n`,
`toList (eval p xs) == ueval p (toList xs)`.

We show this by structural induction on the `RPN` program `p`.

We have 4 cases to consider, one for each constructor of `RPN`.

1. Base case when `p` is `End`.
   Goal: for all `xs :: Vector Double n`,
         toList (eval End xs) == ueval p (toList xs)

   Argument:
   Take any vector xs of type `Vector Double n`. Then,
   toList (eval End xs)   ==    // by eqn 1 of eval
   toList xs              ==    // by eqn 1 of ueval (backwards)
   ueval End (toList xs).

2. Inductive case when `p` is `Enter k rest`.
   Then `rest :: RPN (S n) m`, and we have the
   Inductive hyp: for all `ys :: Vector Double (S n)`,
                  toList (eval rest ys) == ueval rest (toList ys).
   Goal: for all `xs :: Vector Double n`,
         toList (eval (Enter k rest) xs) == ueval (Enter k rest) (toList xs)

   Argument:
   Take any vector xs of type `Vector Double n`. Then `Cons k xs`
   has type `Vector Double (S n)` Thus,
   toList (eval (Enter k rest) xs)   ==  // by eqn 2 of eval
   toList (eval rest (Cons k xs))    ==  // by ind. hyp.
   ueval rest (toList (Cons k xs))   ==  // by eqn 2 of toList
   ueval rest (k:toList xs)          ==  // by eqn 2 of ueval
   ueval (Enter k rest) (toList xs).

3. Inductive case when `p` is `Add rest`, so `p :: RPN (S (S n)) m`.
   Then `rest :: RPN (S n) m`, and we have the
   Inductive hyp: for all `ys :: Vector Double (S n)`,
                  toList (eval rest ys) == ueval rest (toList ys).
   Goal: for all `xs :: Vector Double (S (S n))`,
         toList (eval (Add rest) xs) == ueval (Add rest) (toList xs).

   Argument:
   Take any vector xs of type `Vector Double (S (S n))`. Such a vector
   can be written as xs == Cons x (Cons y zs), where `zs` has type
   `Vector Double n`, so `Cons (y+x) zs` has type `Vector Double (S n)`.

   toList (eval (Add rest) xs)                    ==    // defn of zs
   toList (eval (Add rest) (Cons x (Cons y zs))   ==    // eqn 3 of eval
   toList (eval rest (Cons (y+x) zs))             ==    // ind hyp
   ueval rest (toList (Cons (y+x) zs))            ==    // eqn 2 toList
   ueval rest ((y+x):toList zs)                   ==    // eqn 3 of ueval
   ueval (Add rest) (x:y:toList zs)               ==    // eqn 2 toList
   ueval (Add rest) (x:toList (Cons y zs))        ==    // eqn 2 toList
   ueval (Add rest) (toList (Cons x (Cons y zs))) ==    // defn of zs
   ueval (Add rest) (toList xs).

4. Inductive case when `p` is `AllClear rest`. Then `rest :: RPN Z m`,
   Inductive hyp: for all `ys :: Vector Double Z`,
                  toList (eval rest ys) == ueval rest (toList ys).
   Goal: for all `xs :: Vector Double n`,
         toList (eval (AllClear rest) xs) == ueval (AllClear rest) (toList xs).

   Argument:
   Take any vector `xs` of type `Vector Double n`.
   toList (eval (AllClear rest) xs)    ==  // eqn 4 of eval
   toList (eval rest Nil)              ==  // ind hyp
   ueval rest (toList Nil)             ==  // eqn 1 of toList
   ueval rest []                       ==  // eqn 4 of ueval
   ueval (AllClear rest) (toList xs).

-}
