module Main where

-- PART 1: Python vs. Haskell's IO

int :: String -> Int
int = read

input :: String -> IO String
input prompt = do
  putStr prompt
  getLine

f :: String -> Int
f x = int x - int x
-- f (input "% ") does not typecheck,
-- and so equational reasoning does not break.
-- Compare the Python program `example1.py`.


{-
main :: IO ()
main = do
  i <- input "% "
  let y = f i
  putStrLn (show y)


-}


g :: IO ()
g = do
  putStr "ha"
  putStr "ha"
  putStr "\n"

gP :: IO a -> IO ()
gP x = do
  x
  x
  putStr "\n"
-- `gP (putStr "ha")` does the same thing in main as
-- `g`
-- Compare the Python `example2.py`.


-- PART 2: A simple IO program --

printTriangle' :: Int -> IO ()
printTriangle' 0 = return ()
printTriangle' n = do
  printTriangle' (n - 1)
  putStrLn (replicate n '*')
  

printTriangle :: String -> IO ()
printTriangle xs =
  let i = read xs in
  printTriangle' i
-- 2
-- *
-- **
-- Entry point: the main function in the Main module
-- is the one that gets executed when we run the
-- compiled Haskell program.
-- use `stack ghc Lecture7` to compile, then `./Lecture7` to run
main :: IO ()
main = do
  input <- getLine
  if input == "quit"
  then return ()
  else do
    printTriangle input
    main


-- PART 3: The Either Monad --

-- Left x  >>= (\y -> rest y)   == Left x
-- Right x >>= (\y -> rest y)   == rest x

-- Computation of the rest of the program stops
-- if a `Left` value is encountered. Similar to Maybe, where

-- Nothing >>= (\y -> rest y)   == Nothing
-- Just x  >>= (\y -> rest y)   == rest x

-- But `Left` allows us to have a meaningful error value.

safeDiv :: Int -> Int -> Either String Int
safeDiv x 0 =
  Left ("You tried to divide by 0 in the expr div " ++ show x ++ " 0")
safeDiv x y =
  Right (x `div` y)

-- running this produces a descriptive error
safeDivTwice1 = do
  x <- safeDiv 4 2         -- 4/2 is 2
  y <- safeDiv x (x - 2)   -- 2/(2-2) would be 2/0, an error
  return (y + 1)           -- this is never reached

-- running this produces the expected result:
safeDivTwice2 = do 
  x <- safeDiv 8 2         -- 8/2 is 4
  y <- safeDiv x (x - 2)   -- 4/(4-2) is 4/2, which is 2
  return (y + 1)           -- 2 + 1 is 3, so we get Right 3.
