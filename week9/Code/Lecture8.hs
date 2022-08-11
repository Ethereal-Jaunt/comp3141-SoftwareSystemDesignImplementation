module Lecture8 where

import System.IO

data DistanceKM
  = DistanceKM Double
  deriving (Show, Eq)

data DistanceMi
  = DistanceMi Double
  deriving (Show, Eq)

(+#) :: DistanceKM -> DistanceKM -> DistanceKM
DistanceKM x +# DistanceKM y = DistanceKM (x + y)

(+##) :: DistanceMi -> DistanceMi -> DistanceMi
DistanceMi x +## DistanceMi y = DistanceMi (x + y)

data AreaKM2
  = AreaKM2 Double
  deriving (Show, Eq)

(*#) :: DistanceKM -> DistanceKM -> AreaKM2
DistanceKM x *# DistanceKM y = AreaKM2 (x * y)


{- PHANTOM TYPES -}

data KM
data Mi


data DoubleUnit unit = DoubleUnit Double deriving (Show, Eq)

(+.) :: DoubleUnit a -> DoubleUnit a -> DoubleUnit a
DoubleUnit x +. DoubleUnit y = DoubleUnit (x + y)

km5 :: DoubleUnit KM
km5 = DoubleUnit 5

mi5 :: DoubleUnit Mi
mi5 = DoubleUnit 5


data Sq a

(*.) :: DoubleUnit a -> DoubleUnit a -> DoubleUnit (Sq a)
DoubleUnit x *. DoubleUnit y = DoubleUnit (x * y)

-- Demo 2: Student IDs


{-
  data StudentID = ZID String deriving (Show, Eq)
  getFullName :: (MonadDB db) => StudentID -> db Name
  getWAM :: (MonadDB db) => StudentID -> db Double
  
  enrollIn3141 :: (MonadDB db) => StudentID -> db (Maybe Error)

  enrollIn3141 :: (MonadDB db) => StudentID -> db (Maybe Error)
  enrollIn3141 zid = do
    isUG <- checkUG zid
    if isUG
      then addToCourse zid cs3141 >> return Nothing
      else return (Just notUGError)

  "parse don't validate" is violated by this piece of code.

  We're having to validate the ZID becuase our types could not
  express that we should only ever take an undergrad as input.
-}

data UG
data PG

data StudentID status = ZID String deriving (Show, Eq)


getFullName :: StudentID a -> IO String
getFullName zid
  | zid == ZID "1" = return "Alice"
  | zid == ZID "2" = return "Bob"
  | zid == ZID "3" = return "Mallory"
  | otherwise = return "<UNKNOWN NAME>"

enrollIn3141 :: StudentID UG -> IO ()
enrollIn3141 zid = return () --blah

parseZID :: String -> Either (StudentID UG) (StudentID PG) 
parseZID input
  | input == "1" = Right $ ZID "1"
  | otherwise = Left $ ZID input

process :: String -> IO ()
process input =
  case parseZID input of
    Left zid -> do -- UG branch
      enrollIn3141 zid
      name <- getFullName zid
      putStrLn $ name ++ " was successfully enrolled in COMP3141."
    Right zid -> do -- PG branch
      name <- getFullName zid
      putStrLn $ name ++ " CANNOT BE ENROLLED IN COMP3141."

main = do
  putStr "Give me a zID: "
  x <- getLine
  process x
  putStrLn ""
  main


-- to solve the weirdStudent problem, we'll enable language extensions.
-- DataKinds extension
-- KindSignatures extension
-- see Lecture8DK.hs

-- Demo 3: Safer file IO via Phantom Types

data SRmode
data SRWmode
data SafeHandle mode = SafeHandle Handle

openR :: FilePath -> IO (SafeHandle SRmode)
openR fp = do
  h <- openFile fp ReadMode
  return (SafeHandle h)

openRW :: FilePath -> IO (SafeHandle SRWmode)
openRW fp = do
  h <- openFile fp ReadWriteMode
  return (SafeHandle h)

sPutStrLn :: SafeHandle SRWmode -> String -> IO ()
sPutStrLn (SafeHandle h) str = hPutStrLn h str

sClose :: SafeHandle a -> IO ()
sClose (SafeHandle h) = hClose h



{-
 You should use phantom types precisely when
 1. certain functions should only work when the phantom argument
     has a specific type
 2. and other functions should work no matter what type the
    phantom argument has

 The only exception: when you're using other advanced language
 features, such as GADTs.
-}


-- Demo 4: Making illegal states unrepresentable

{-
  Writing a soccer program...

  You want to represent a log of the current play.

  -- Pass: kick the ball from teammate to teammate
  -- Steal: taken from the current player by someone from the other team
  -- Goal: the current player scores a goal
  -- Out: the ball leaves the play area
-}

data Matildas = SamKerr | CaitilinFoord deriving (Show, Eq)
data Ferns = LilyAlfeld deriving (Show, Eq)

data Play t1 t2  -- t1 always the team currently possessing the ball
  = Start t1
  | Pass t1 (Play t1 t2)
  | Steal t1 (Play t2 t1)
  deriving (Show, Eq)

-- data FinishedPlay t1 t2 =
--   | Goal (Play t1 t2)
--   | Out (Play t1 t2)
--   deriving (Show, Eq)

game :: Play Matildas Ferns
game = Start SamKerr

{-

  Warning: there will be no exercise for phantom types and
  making illegal states unrepresentable. But you'll need to get
  good at it, as this is all assessable content. We might ask
  you to create or decide between different types in the exam.



  Bonus problem 1:

  a. Can you model FinishedPlay using a phantom
     type argument, `Play state t1 t2`, with
     `Play Finished t1 t2` behaving like `FinishedPlay`?
  b. Should you try?
  c. If you can't, why not? What additional things would you need?



  Bonus problem 2:

  a. A client-server file transfer protocol supports three operations:
     `request`: the client sends a list of file names to the server
     `fulfill`: responding to a request, the server sends a file
                to the client.
     `fnotfnd`: responding to a request, the server tells the client
                that it does not have the requested file.

  b. First, the client can send a single request. Then, the server
     responds to the request. If the server has all the requested
     files, it sends multiple `fulfill` responses, one for each file.
     Otherwise, the server responds with a single `fnotfnd` response
     informing the client of one of the files that were not found.

     E.g. the following could be a valid log of the protocol.
       C> request hello.txt goodbyte.txt
       S> fnotfnd goodbyte.txt
       C> request hello.txt goodbye.txt
       S> fulfill hello.txt "hello world!"
       S> fulfill goodbye.txt "good bye!"

     But the following could not be a valid log:
       C> request hello.txt goodbyte.txt
       S> fnotfnd hello.txt
       S> fnotfnd goodbyte.txt
       C> request hello.txt
       C> request bello.txt
     (because the server sent two fnotfnd responses, and also
      because the client didn't wait for a response and sent
      two requests in a row)

  c. Design types for representing valid logs of the protocol, while
     making as many invalid logs unrepresentable as possible.



  Bonus problem 3:

  Find an assurance problem that can be solved with phantom types,
  and sketch such a solution.
-}
