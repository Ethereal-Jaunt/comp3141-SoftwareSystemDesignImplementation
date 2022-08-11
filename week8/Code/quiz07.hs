-- data Direction = L | R
-- forward    :: IO ()
-- obstructed :: IO Bool
-- turn       :: Direction -> IO ()


-- robot = do
--   sensed <- obstructed
--   if sensed 
--     then turn L
--     else forward
--   robot

-- >>=

-- robot = do
--   sensed <- obstructed
--   if sensed
--     then do
--          turn L
--          robot
--     else do
  --        forward
--          robot
-- >> = >>= \_ -> m a

------------------------------Q4----------------------------

-- f41 [] = return ""
-- f41 (x:xs) = putChar x >>= \_ -> xs

f42 :: String -> IO ()
f42 [] = return ()
f42 (x:xs) = putChar x >>= \_ -> f42 xs


f43 [] = return ()
f43 (x:xs) = putChar x >>= \xs -> f43 (show xs)

f44 xs = case xs of
  [] -> return ()
  (x:xs) -> f44 xs >>= \_ -> putChar x

{--
return :: a-> IO a
fmap :: (a->b) -> IO a -> IO b
(<*>) :: IO(a->b) -> IO a -> IO b

 b :: IO (a -> b) -> a -> IO b 

 return a :: IO a
 
bind(>>=) :: IO a -> (a-> IO b) -> IO b 


-}


--------------------------Q7-------------------------
-- fold操作是把一个列表聚合成一个值的过程,而在此基础上有foldl和foldr两种对称的实现。两个函数的一种定义如下：

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- foldl f _ [] = _
-- foldl f z (x:xs) = foldl f (f z x) xs

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldr f _ [] = _
-- foldr f z (x:xs) = f x (foldr f z xs)

-- foldl (+) 1 [1,2,3]
-- foldl (++) [] [[1],[2],[3]]



-- const x is a unary function which evaluates to x for all inputs.
-- >>> const 42 "hello"
-- 42
-- >>> map (const 42) [0..3]
-- [42,42,42,42]