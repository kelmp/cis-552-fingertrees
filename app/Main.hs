module Main where

import FingerTree as FT (fromList, (!!))
import System.Random (randomRIO)
import Text.Read (readMaybe)

newtype Person = Person {
    name :: String
    bio :: String
}

durTree :: FingerTree String
durTree = FT.fromList ["seconds", "minutes", "hours", "days", "weeks", "months",
  "years", "decades", "centuries", "millenia"]

nameTree :: FingerTree String
nameTree = FT.fromList ["Alice", "Bob", "Carol", "Dave", "Eve", "Frank",
  "Grace", "Heidi", "Ivan", "Judy", "Mallory", "Oscar", "Peggy", "Rupert",
  "Sybil", "Ted", "Victor", "Walter", "Wendy", "Adrian", "Yoni"]

adjTree :: FingerTree String
adjTree = FT.fromList ["loyal", "satisfied", "happy", "star",
  "aggressively nice", "friendly"]

newPerson :: IO Person
newPerson = do
  nameI <- randomRIO (0, length nameTree - 1)
  adjI <- randomRIO (0, length adjTree - 1)
  durI <- randomRIO (0, length timeTree - 1)
  time <- randomRIO (2, 9)
  let name = nameTree !! nameI
      adj = adjTree !! adjI
      dur = durTree !! durI in
  return { name = name
    bio = name ++ " has been a " ++ adj ++ " customer of Not-Disney for " ++
      time ++ " " ++ dur ++ ", but that's going to change if they can't speak\
        \to a manager RIGHT NOW."}

addPerson :: Bool -> FingerTree Person -> FingerTree Person
addPerson fastpass ft = do
  p <- newPerson
  if fastpass then insertHead p ft else insertTail p ft

init :: Int -> FingerTree Person
init n = FT.fromList $ iterate (addPerson False) Nil !! n

getInt = do
  input <- getLine
  case (readMaybe input :: Maybe Int) of
    Just i -> i
    Nothing -> getInt


main :: IO ()
main = do
  putStrLn "Welcome to Not-Disney Not-Land-Or-World! How many people have \
  \rushed into the renowned Cosmos-Themed Landform Ride as soon as it opened?"
