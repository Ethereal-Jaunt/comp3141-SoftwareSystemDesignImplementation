module Lecture5 where

import Data.Char

-------------------------------------------------------
--DEMO 1--

data ID = ID Integer deriving (Show, Eq)

data Employee = Employee
  { idNumber :: ID
  , name :: String
  , supervisor :: Maybe ID
  } deriving (Show, Eq)

database :: [Employee]
database =
--           id     name              supervisor
  [ Employee (ID 1) "Jane C. Founder" Nothing
  , Employee (ID 2) "Robert Sysadmin" (Just (ID 1))
  , Employee (ID 3) "Simon Singh"     (Just (ID 1))
  , Employee (ID 4) "Obvious E. Rorr" (Just (ID 5))
  ]

dbLookup :: ID -> Maybe Employee
dbLookup i = case filter (\x -> idNumber x == i) database of
  (e:_) -> Just e
  []    -> Nothing

-- parseID "2" -> Just (ID 2)
parseID :: String -> Maybe ID
parseID [] = Nothing
parseID xs
  | all isNumber xs = Just . ID . read $ xs
  | otherwise       = Nothing


-- return the supervisor of the employee with the given ID
-- assuming that the employee exists, the employee has a supervisor,
-- and finally that the supervisor is valid.
-- getSupervisor "3"
-- ID 3
-- 去数据库里查找ID3
-- 获取supervisor
-- Just (Employee {idNumber = ID 1, name = "Jane C. Founder", supervisor = Nothing})
getSupervisor :: String -> Maybe Employee
getSupervisor userInput =
  case parseID userInput of
    Nothing -> Nothing
    Just i -> case dbLookup i of
      Nothing -> Nothing
      Just e  -> case supervisor e of
        Nothing -> Nothing
        Just si -> dbLookup si

getSupervisor1 :: String -> Maybe Employee
getSupervisor1 userInput =
  bindM (parseID userInput) $ \i -> bindM (dbLookup i) $ \e -> bindM (supervisor e) dbLookup

getSupervisor2 :: String -> Maybe Employee
getSupervisor2 userInput = (parseID userInput) >>= dbLookup >>= supervisor >>= dbLookup

getSupervisor3 :: String -> Maybe Employee
getSupervisor3 userInput = bindM (bindM (bindM (parseID userInput) dbLookup) supervisor) dbLookup

-- recurring pattern:
--  either we get Nothing and return Nothing
--  or else we get something (i, e, si) and then continue with that
--
{-
       variable part: what we're testing
       vvvvvvvvvvvvvvvvv
  case parseID userInput of
    Nothing -> Nothing -- return Nothing
    Just i ->  rest_of_the_program i -- the rest of the program is
                                     -- a function of i

               ^^^^^^^^^^^^^^^^^^^ the rest of the program
-}

bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
bindM value rest =
  case value of
    Nothing -> Nothing
    Just v  -> rest v

{-
bindM' :: Maybe a -> (a -> Maybe b) -> Maybe b
bindM' Nothing rest = Nothing
bindM' (Just v) rest = rest v
-}
{-

getSupervisor :: String -> Maybe Employee
getSupervisor userInput =
  case parseID userInput of
    Nothing -> Nothing
    Just i -> case dbLookup i of
      Nothing -> Nothing
      Just e  -> case supervisor e of
        Nothing -> Nothing
        Just si -> dbLookup si

-}
-- bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
getSupervisorB :: String -> Maybe Employee
getSupervisorB userInput =
  --     Maybe ID
  bindM (parseID userInput) $ \i  ->
  bindM (dbLookup i)        $ \e  ->
  bindM (supervisor e)      $ \si ->
  dbLookup si



-------------------------------------------------------
--DEMO 2--

data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

{-
labeling the tree
    o
   / \
  o   o
    

should yield
    1
   / \
  2   3
    
-}

exampleTree :: Tree ()
exampleTree = Node () (Node () Leaf Leaf)
                      (Node () Leaf Leaf)
-- Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf))
slideTree :: Tree ()
slideTree = Node () (Node () (Node () Leaf Leaf) (Node () Leaf Leaf))
                    (Node () (Node () Leaf Leaf) (Node () Leaf Leaf))

label :: Tree a -> Tree Integer
label tree = snd (go tree 1) where
  go :: Tree a -> Integer -> (Integer, Tree Integer)
  go Leaf         = \lun -> (lun, Leaf)  
  go (Node v l r) = \lun ->
    let (lun', l') = go l (lun + 1) in
    let (lun'', r'') = go r lun' in
    (lun'', Node lun l' r'')


--   label exampleTree
-- = label (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))
-- = snd (go (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) 1)

--        go exampleTree 1
--      = go (Node () (Node () Leaf Leaf) (Node () Leaf Leaf)) 1
--              let (lun', l') = go l (lun + 1) 
--                       = go (Node () Leaf Leaf) (1+1)
--                        let (lun',l') = go l (lun+1) = go Leaf 3 = (3,Leaf)
--                        let (lun'',r'') = go r lun' = go Leaf 3 =(3,Leaf)
--                        in (lun'' , Node lun l' r'')
--                       = (3, Node 2 Leaf Leaf)
--              let (lun'', r'') = go r lun' 
--                       = go (Node () Leaf Leaf) 3
--                        let (lun',l') = go l (lun+1) = go Leaf 4 = (4,Leaf)
--                        let (lun'',r'') = go r lun' = go Leaf 4 = (4,Leaf)
--                        in (lun'',Node lun Leaf Leaf)
--                       =(4,Node 3 Leaf Leaf)
--       
--              in (lun'', Node lun l' r'')
--              = (4,Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))
--        
--

----------------------------------------------------------------------------------------------------
--   label slideTree
--   slideTree = Node () (Node () (Node () Leaf Leaf) (Node () Leaf Leaf))
--                       (Node () (Node () Leaf Leaf) (Node () Leaf Leaf))
-- = label slideTree
-- = snd . go slideTree 1

--        go slideTree 1
--      = go ( Node () ( Node () (Node () Leaf Leaf) (Node () Leaf Leaf) )
--                     ( Node () (Node () Leaf Leaf) (Node () Leaf Leaf) ) ) 1
--              let (lun', l') = go l (lun + 1) 
--                       = go Node () (Node () Leaf Leaf) (Node () Leaf Leaf) (1+1)
--                          let (lun', l') = go l (lun + 1) 
--                                   = go (Node () Leaf Leaf) (2+1)
--                                    let (lun',l') = go l (lun+1) = go Leaf 4 = (4,Leaf)
--                                    let (lun'',r'') = go r lun' = go Leaf 4 =(4,Leaf)
--                                    in (lun'' , Node lun l' r'')
--                                   = (4, Node 3 Leaf Leaf)
--                          let (lun'', r'') = go r lun' 
--                                   = go (Node () Leaf Leaf) 4
--                                    let (lun',l') = go l (lun+1) = go Leaf 5 = (5,Leaf)
--                                    let (lun'',r'') = go r lun' = go Leaf 5 = (5,Leaf)
--                                    in (lun'',Node 3 Leaf Leaf)
--                                   =(5,Node 4 Leaf Leaf)
--                 
--                          in (lun'', Node lun l' r'')
--                          = (5,Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))
--              let (lun'', r'') = go r lun' 
--                       = go Node () (Node () Leaf Leaf) (Node () Leaf Leaf) (5)
--                          let (lun', l') = go l (lun + 1) 
--                                   = go (Node () Leaf Leaf) (5+1)
--                                    let (lun',l') = go l (lun+1) = go Leaf 7 = (7,Leaf)
--                                    let (lun'',r'') = go r lun' = go Leaf 7 =(7,Leaf)
--                                    in (lun'' , Node lun l' r'')
--                                   = (7, Node 6 Leaf Leaf)
--                          let (lun'', r'') = go r lun' 
--                                   = go (Node () Leaf Leaf) 7
--                                    let (lun',l') = go l (lun+1) = go Leaf 8 = (8,Leaf)
--                                    let (lun'',r'') = go r lun' = go Leaf 8 = (8,Leaf)
--                                    in (lun'',Node lun l' r'')
--                                   =(8,Node 7 Leaf Leaf)
--                 
--                          in (lun'', Node lun l' r'')
--                          = (8,Node 5 (Node 6 Leaf Leaf) (Node 7 Leaf Leaf))
--       
--              in (lun'', Node lun l' r'')
--              = (8,Node 1 (Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 5 (Node 6 Leaf Leaf) (Node 7 Leaf Leaf)))


label1 :: Tree a -> (Integer,Tree Integer)
label1 tree = (go tree 1) where
  go :: Tree a -> Integer -> (Integer, Tree Integer)
  go Leaf         = \lun -> (lun, Leaf)  
  go (Node v l r) = \lun ->
    let (lun', l') = go l (lun + 1) in
    let (lun'', r'') = go r lun' in
    (lun'', Node lun l' r'')

{-                state       state
                  vvvvvvv     vvvvvvv
  type signature: Integer -> (Integer, Tree Integer)
                                       ^^^^^^^^^^^^
                                       return value
  -- final return value: Tree Integer
  -- a global state that we maintain is an Integer value

  \origState ->
    let (newState, retVal1) = ... in
    let (newerState, retVal2) = ... in
    let (newestState, retVal3) = ... in
    ...
-}

type State s a = s -> (s, a)
-- a: eventual return value
-- s: state
---                                        State s a
bindS :: State s a -> (a -> State s b) -> s -> (s, b)
-- f: State s a   rest: (a -> State s b)  s: \origState
bindS f rest = \origState ->
  let (newState, retVal) = f origState in
  rest retVal newState
-- rest :: a -> State s b = a -> s -> (s,a)
-- rest retVal newState :: (s,a)
-- bindS f rest = \origState -> (s,a)

-- helpful State combinators

-- returns the state without changing it
-- get :: s -> (s,a)
get :: State s s
get = \origState -> (origState, origState)

-- replace the state with the argument (update the state)
-- put :: s -> s -> (s,a)
put :: s -> State s ()
put newState = \origState -> (newState, ())

-- the function that returns its argument, and leaves the state unchanged
-- yield :: a -> s -> (s,a)
-- yield v = \x -> (x,v)
yield :: a -> State s a
yield v = \origState -> (origState, v)

-- use gets the least unused number, and returns it as a value for
-- the rest of the program
-- and also, it will update the least unused number to a larger one.
-- use :: Integer -> (Integer,Integer)
use :: State Integer Integer
use = \lun -> (lun + 1, lun)
-- use :: Integer -> (Integer, Integer)


use' :: State Integer Integer
use' =
  bindS get              $ \lun ->
  bindS (put (lun + 1))  $ \()  ->
  yield lun

{-

label :: Tree a -> Tree Integer
label tree = snd (go tree 1) where
  go :: Tree a -> Integer -> (Integer, Tree Integer)
  go Leaf         = \lun -> (lun, Leaf)
  go (Node v l r) = \lun ->
    let (lun', l') = go l (lun + 1) in
    let (lun'', r'') = go r lun' in
    (lun'', Node lun l' r'')

-}
--bindS :: State s a -> (a -> State s b) -> s -> (s, b)
label' :: Tree a -> Tree Integer
label' tree = snd (go tree 1) where
  go :: Tree a -> State Integer (Tree Integer)
  go Leaf = yield Leaf
  go (Node x l r) =
    bindS use     $ \lun ->
    bindS (go l)  $ \l'  ->
    bindS (go r)  $ \r'' ->   --State Integer (Tree Integer)
    yield (Node lun l' r'') -- State s a

-- go (Node x l r) = bindS use (\lun -> bindS (go l) (\l' -> bindS (go r) (\r'' -> yield (Node lun l' r''))))
{-

bindS :: State s a -> (a -> State s b) -> s -> (s, b)
-- f: State s a   rest: (a -> State s b)  s: \origState
bindS f rest = \origState ->
  let (newState, retVal) = f origState in
  rest retVal newState

-}
{-
bindM :: Maybe   a -> (a -> Maybe b)   -> Maybe b
bindS :: State s a -> (a -> State s b) -> State s b

bind  :: m a -> (a -> m b)       -> m b


-}


tryNumber :: Int -> Maybe Int
tryNumber x
  | even x = Just x
  | otherwise = Nothing

toString :: Int -> String
toString = show

-- fmap :: (Functor f) => (a -> b) -> f a -> f b
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f x = case x of
  Nothing -> Nothing
  Just v  -> Just (f v)
-- maybeMap (\x -> x+1) (Just 3)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f x = case x of
  Leaf -> Leaf
  (Node v l r) -> Node (f v) (treeMap f l) (treeMap f r)
-- label' exampleTree
-- treeMap (\x -> x^2) $ label' exampleTree

type Fun x = Integer -> x

-- funMap :: (a->b) -> (Fun a) -> (Fun b)
funMap :: (a -> b) -> (Integer -> a) -> Integer -> b
--           f             g              x
funMap f g x = f (g x)


-- Int
-- Maybe * ->Maybe *

-- Maybe 
-- Nothing 
-- Just a 
-- Just 2
-- fmap :: m a -> (a->b) -> m b
-- fmap Nothing = Nothing
-- fmap Just a = Just (f a)

-- Gen a
-- <*>


-- ==>
-- Just 2 -> (\x -> Just (x+1) ) -> Just 3 
-- bind :: m a -> (a->m b) -> m b

