




s2 :: Monad m => [m a] -> m [a]
s2 [] = return []
s2 (a:as) = do
  x <- a
  xs <- s2 as
  return (x : xs)
-- Just 1
-- Maybe
-- [Maybe Int]
-- s [Just 1,Just 2,Just 4] = Just [1,2,4]

s3 :: Monad m => [m a] -> m [a]
s3 [] = return []
s3 (a:as) = do
  x <- a
  xs <- s3 as
  return (x : xs)

-- s4 :: Monad m => [m a] -> m [a]
-- s4 [] = return []
-- s4 (a:as) = do
--   a
--   s4 as
--   return (a : as)


------------------------------Q4---------------------------
data NonEmptyList a = One a | Cons a (NonEmptyList a)

instance Functor NonEmptyList where
   fmap = map

instance Applicative NonEmptyList where
   pure x = Cons x (pure x)
   (One f) <*> (One x) = One (f x)
   (One f) <*> (Cons x _) = One (f x)
   (Cons f _) <*> (One x) = One (f x)
   (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)




------------------------------Q8---------------------------
data Info a = Info a deriving (Show, Eq)

instance Functor Info where
   -- fmap f = (<$>) f       -- 2
--   fmap f xs = f <*> xs   -- 3
   fmap f xs =
        xs >>= \x -> return (f x) -- 4
--   fmap f (Info x) = Info (f x)

instance Applicative Info where
   pure x = Info x
   (Info f) <*> (Info x) = Info (f x )

instance Monad Info where
   (Info x) >>= f = f x 



-- (,) a for any a
-- instance Applicative ((,) x) where
--     pure :: a -> (x,a)
--     pure a = (???,a)

--pair fa fb = pure (,) <*> fa <*> fb
            -- = f (,) <*> fa <*> fb
            -- = \x->f (a,x) <*>fb
            -- = f (a,b)

--pair fa fb = fmap (,) fa <*> fb
--              = \x->f (a,x) <*>fb
            -- = f (a,b)


--Every monad m and every function f :: a -> m a satisfies the equality f <=< f = f
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
-- (f <=< g) x = g x >>= \gx -> f gx

-- Just 1  f x = Just (x+1) != Just (x+2)
-- f <=< f x = f x
-- \x -> Just (x+1) <=< \x -> Just (x+1)
-- f x >>= \gx -> f gx
-- Just (x+1) >>= \gx -> f gx
-- f (x+1) = Just (x+1 +1) = Just (x+2)
