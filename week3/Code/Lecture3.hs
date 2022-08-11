module Lecture3 where

import Data.Semigroup
import Test.QuickCheck

data MySum = MySum Int deriving (Show, Eq)

instance Semigroup MySum where
  (MySum x) <> (MySum y) = MySum (x + y)
instance Monoid MySum where
  mappend = (<>)
  mempty = (MySum 0)

data Prod = Prod Int deriving (Show, Eq)

instance Semigroup Prod where
  (Prod x) <> (Prod y) = Prod (x * y)
instance Monoid Prod where
  mappend = (<>)
  mempty = Prod 1

expo :: (Monoid g) => g -> Int -> g
expo x 0 = mempty
expo x n = x <> expo x (n-1) where
  (<>) = mappend

fexpo :: (Monoid g) => g -> Int -> g
fexpo x 0 = mempty
fexpo x n
  | even n = let y = fexpo x (n `div` 2) in mappend y y
  | otherwise = mappend x (fexpo x (n - 1))

-- expo (prod 2) 10
-- fexpo (Prod 2) 10
-- length $ expo "hello " (2^20)
-- length $ fexpo "hello " (2^20)


prop_reverseApp :: String -> String -> Bool
prop_reverseApp xs ys =
  reverse (xs ++ ys) == (reverse ys) ++ (reverse xs)


--shrink "hello"

data RPS = R | P | S deriving (Show, Eq)

(<|>) :: RPS -> RPS -> RPS
R <|> P = P
R <|> S = R
P <|> S = S
P <|> R = P
S <|> R = R
S <|> P = S
x <|> y = x

prop_assoc :: RPS -> RPS -> RPS -> Bool
prop_assoc x y z = (x <|> y) <|> z  ==  x <|> (y <|> z)
-- should fail

prop_comm :: RPS -> RPS -> Bool
prop_comm x y = x <|> y == y <|> x
-- should pass


instance Arbitrary RPS where
  arbitrary = oneof [pure R, pure P, pure S]


data Color = Color Int Int Int deriving (Show, Eq)

color x y z = Color (x `mod` 256) (y `mod` 256) (z `mod` 256)

prop_color :: Color -> Bool
prop_color x = (x == x)

prop_invar :: Color -> Bool
prop_invar (Color r g b) = r < 256

instance Arbitrary Color where
  arbitrary = color <$> arbitrary <*> arbitrary <*> arbitrary
  -- color arbitrary arbitrary arbitrary -- Gen Color（我们想要的）
