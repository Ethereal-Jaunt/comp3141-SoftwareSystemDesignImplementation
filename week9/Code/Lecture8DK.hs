{-# LANGUAGE DataKinds, KindSignatures #-}

module Lecture8 where

data Status = UG | PG
data Student (status :: Status) = ZID String deriving (Show, Eq)

parseZID :: String -> Either (Student UG) (Student PG)
parseZID zid
  | zid `elem` ["1","2","3"] = Right (ZID zid)
  | otherwise                = Left (ZID zid)

enrollIn3141 :: Student UG -> IO ()
enrollIn3141 zid = return () -- blah

getFullName :: Student a -> IO String
getFullName zid = return "<NAME HERE>" -- blah

test :: String -> IO ()
test input =
  case parseZID input of
    Left zid -> do
      name <- getFullName zid
      putStrLn $ name ++ " can enroll in 3141."
      enrollIn3141 zid
      putStrLn "Enrolled. Next!"
    Right zid -> do
      name <- getFullName zid
      putStrLn $ name ++ " can't enroll in 3141."
      putStrLn "Next!"


weirdStudent :: Student String
weirdStudent = ZID "won't typecheck"


main :: IO ()
main = do
  putStr "Student ID: "
  x <- getLine
  test x
  putStrLn ""
  main

