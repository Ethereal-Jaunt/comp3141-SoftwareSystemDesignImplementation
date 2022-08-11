module Lecture2 where

import Data.Semigroup

data Document
  = Passport
  | BirthCert
  | License
  | StudentID
  | CreditCard
  deriving (Show, Eq)

data Category
  = Primary
  | Secondary
  | Tertiary deriving (Show, Eq)

category :: Document -> Category
category Passport = Primary
category BirthCert = Primary
category License = Secondary
category StudentID = Secondary
category _ = Tertiary

points :: [Document] -> Int
points docs =
  70*primaryCounts +
  25*tertiaryCounts +
  secondaryPoints secondaryDocuments where
  primaryCounts = length $ filter (\x -> category x == Primary) docs
  tertiaryCounts = length $ filter (\x -> category x == Tertiary) docs
  secondaryDocuments = filter (\x -> category x == Secondary) docs
  secondaryPoints :: [Document] -> Int
  secondaryPoints [] = 0
  secondaryPoints (x:xs) = 40 + (25 * length xs)

data MonthDayType = MonthDayConstructor Int Int deriving (Show, Eq)

showMonthDay :: MonthDayType -> String
showMonthDay (MonthDayConstructor m d) =
    "the day is " ++ show d ++ " and the month is " ++ show m

-- data Maybe a = Nothing | Just a

divide :: Int -> Int -> Maybe Int
divide x 0 = Nothing
divide x y = Just (x `div` y)

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)

length' :: List a -> Int
length' Nil = 0
length' (Cons x xs) = 1 + length' xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

addFirstElements :: [Int] -> [Int] -> Maybe Int
addFirstElements xs ys = case safeHead xs of
  Nothing -> Nothing
  Just x -> case safeHead ys of
    Nothing -> Nothing
    Just y -> Just (x + y)


data NonEmpty a
  = One a
  | ConsNE a (NonEmpty a)
  deriving (Show, Eq)

toList :: NonEmpty a -> [a]
toList (One x) = [x]
toList (ConsNE x xs) = x: toList xs

safeHead' :: NonEmpty a -> a
safeHead' (One x) = x
safeHead' (ConsNE x _) = x

class ShortShow a where
  shortShow :: a -> Char

instance ShortShow Bool where
  shortShow True = 'T'
  shortShow False = 'F'

instance Show a => ShortShow (Maybe a) where
  shortShow x = head (show x)

