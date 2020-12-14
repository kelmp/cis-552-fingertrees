{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Prelude hiding (tail, fromList)
import FingerTree as FT (FingerTree (..), fromList, (!!), insertHead,
  insertTail, Measured (..), head, tail)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, fromJust)

default (Int)

data Person = Person
  { name :: String,
    adj :: String,
    memberFor :: String,
    fastPass :: Bool }
  deriving (Eq, Show)

instance Measured String
instance Measured Person

durTree :: FingerTree String
durTree =
  FT.fromList
    [ "seconds",
      "minutes",
      "hours",
      "days",
      "weeks",
      "months",
      "years",
      "decades",
      "centuries",
      "millenia"
    ]

nameTree :: FingerTree String
nameTree =
  FT.fromList
    [ "Alice",
      "Bob",
      "Carol",
      "Dave",
      "Eve",
      "Frank",
      "Grace",
      "Heidi",
      "Ivan",
      "Judy",
      "Mallory",
      "Oscar",
      "Peggy",
      "Rupert",
      "Sybil",
      "Ted",
      "Victor",
      "Walter",
      "Wendy",
      "Adrian",
      "Yoni"
    ]

adjTree :: FingerTree String
adjTree =
  FT.fromList
    [ "loyal",
      "satisfied",
      "happy",
      "star",
      "aggressively nice",
      "friendly",
      "obsessive",
      "dangerously frugal"
    ]

newPerson :: IO Person
newPerson = do
  nameI <- randomRIO (0, measure nameTree - 1)
  adjI <- randomRIO (0, measure adjTree - 1)
  durI <- randomRIO (0, measure durTree - 1)
  time <- randomRIO (2, 9)
  return Person
    { name = fromJust $ nameTree FT.!! nameI,
      adj = fromJust $ adjTree FT.!! adjI,
      memberFor = show time ++ " " ++ fromJust (durTree FT.!! durI),
      fastPass = time `mod` 3 == 0
    }

addPerson :: FingerTree Person -> IO (FingerTree Person)
addPerson ft = do
  p <- newPerson
  if fastPass p then
    return $ insertHead p ft else
      return $ insertTail p ft

verboseAddPerson :: FingerTree Person -> IO (FingerTree Person)
verboseAddPerson ft = do
  p <- newPerson
  if fastPass p then do
    putStrLn $ name p ++ " is being added to the front (thanks, fastpass!)"
    return $ insertHead p ft else do
      putStrLn $ name p ++
        " is being added to the end because they're poor like me :("
      return $ insertTail p ft

addPeople :: Int -> FingerTree Person -> IO (FingerTree Person)
addPeople 0 ft = return ft
addPeople n ft = do
  ft' <- verboseAddPerson ft -- toggle verbose here --
  addPeople (n - 1) ft'

admitPeople :: Int -> FingerTree Person -> IO (FingerTree Person)
admitPeople 0 ft = return ft
admitPeople _ Nil = return Nil
admitPeople n ft = do
  let h = fromJust $ FT.head ft in do
    putStrLn $ name h ++ " made it into the ride!"
    admitPeople (n - 1) (tail ft)

complain' :: Int -> FingerTree Person -> String
complain' i ft = case ft FT.!! i of
  Nothing -> error "Invalid customer index for complaining"
  Just p -> name p ++ " is #" ++ show (i + 1) ++ " in line, which is frankly \
    \UNACCEPTABLE. As a " ++ adj p ++ " customer of Not-Disney for "
    ++ memberFor p ++ ", something bad is going to happen if they can't speak \
      \to a manager RIGHT NOW."

complain :: Person -> String
complain p =
  name p ++ " has been a " ++ adj p ++ " customer of Not-Disney for "
    ++ memberFor p
    ++ ", but that's going to change if they can't speak \
       \to a manager RIGHT NOW."

getPosInt :: IO Int
getPosInt = do
  input <- getLine
  -- maybe getInt return (readMaybe input :: Maybe Int)
  case (readMaybe input :: Maybe Int) of
    Nothing -> getPosInt
    Just x -> if x > 0 then return x else getPosInt

getIntBounded :: Int -> Int -> IO Int
getIntBounded lo hi = do
  input <- getLine
  case (readMaybe input :: Maybe Int) of
    Nothing -> getIntBounded lo hi
    Just x -> if x >= lo && x <= hi then return x else getIntBounded lo hi

queueLoop :: FingerTree Person -> IO ()
queueLoop q =
  let len = measure q in do
    putStrLn $ "There are currently " ++ show len ++ " people in the queue."
    angryI <- randomRIO (0, measure q - 1)
    -- putStrLn $ complain $ fromJust $ q FT.!! angryI
    putStrLn $ complain' angryI q
    putStrLn "Please type your response to this well-behaved customer."
    -- input <- getLine
    getLine
    putStrLn
      "After hearing your soothing words, the customer heads back to their\
      \ spot in line."
    putStrLn $ "How many people would you like to let into the ride? (0-" ++
      show (measure q) ++ ")"
    toAdmit <- getIntBounded 0 (measure q)
    q <- admitPeople toAdmit q
    case q of
      Nil -> putStrLn "The line is empty, so this demo is over. Thanks!"
      _ -> do
        toAdd <- randomRIO(1, 10)
        -- putStrLn $ "queue pre-add: " ++ show (measure q)
        -- putStrLn $ "toAdd: " ++ show toAdd
        q <- addPeople toAdd q
        -- putStrLn $ "queue post-add: " ++ show (measure q)
        queueLoop q

main :: IO ()
main = do
  putStrLn
    "Welcome to Not-Disney Not-Land-Or-World! How many people have \
    \rushed into the renowned Cosmos-Themed Landform Ride as soon as it \
    \opened? (At least 1)"
  startSize <- getPosInt
  queue <- addPeople startSize Nil
  queueLoop queue

-- other idea - lottery-ish? You win more the closer you are to the center
-- let queue = init getInt
