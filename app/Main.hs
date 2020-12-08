<<<<<<< HEAD
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
=======
>>>>>>> 2e4246c0a603aa179f49874443d1b6a62b208d30
{-# LANGUAGE DefaultSignatures #-}

module Main where

import Prelude hiding (tail, fromList)
import FingerTree as FT (FingerTree (..), tail, fromList, (!!), insertHead,
  insertTail, Measured (..))
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, fromJust)

data Person = Person
  { name :: String,
    adj :: String,
    memberFor :: String,
    fastPass :: Bool }

instance Measured String where
  measure s = 1

-- newtype Person = Person
--   { name :: String,
--     adj :: String,
--     memberFor :: String
--   }

-- durTree :: FingerTree String
-- durTree =
--   FT.fromList
--     [ "seconds",
--       "minutes",
--       "hours",
--       "days",
--       "weeks",
--       "months",
--       "years",
--       "decades",
--       "centuries",
--       "millenia"
--     ]

-- nameTree :: FingerTree String
-- nameTree =
--   FT.fromList
--     [ "Alice",
--       "Bob",
--       "Carol",
--       "Dave",
--       "Eve",
--       "Frank",
--       "Grace",
--       "Heidi",
--       "Ivan",
--       "Judy",
--       "Mallory",
--       "Oscar",
--       "Peggy",
--       "Rupert",
--       "Sybil",
--       "Ted",
--       "Victor",
--       "Walter",
--       "Wendy",
--       "Adrian",
--       "Yoni"
--     ]

-- adjTree :: FingerTree String
-- adjTree =
--   FT.fromList
--     [ "loyal",
--       "satisfied",
--       "happy",
--       "star",
--       "aggressively nice",
--       "friendly"
--     ]

<<<<<<< HEAD
=======
-- newtype Person = Person
--   { name :: String,
--     adj :: String,
--     memberFor :: String
--   }

-- durTree :: FingerTree String
-- durTree =
--   FT.fromList
--     [ "seconds",
--       "minutes",
--       "hours",
--       "days",
--       "weeks",
--       "months",
--       "years",
--       "decades",
--       "centuries",
--       "millenia"
--     ]

-- nameTree :: FingerTree String
-- nameTree =
--   FT.fromList
--     [ "Alice",
--       "Bob",
--       "Carol",
--       "Dave",
--       "Eve",
--       "Frank",
--       "Grace",
--       "Heidi",
--       "Ivan",
--       "Judy",
--       "Mallory",
--       "Oscar",
--       "Peggy",
--       "Rupert",
--       "Sybil",
--       "Ted",
--       "Victor",
--       "Walter",
--       "Wendy",
--       "Adrian",
--       "Yoni"
--     ]

-- adjTree :: FingerTree String
-- adjTree =
--   FT.fromList
--     [ "loyal",
--       "satisfied",
--       "happy",
--       "star",
--       "aggressively nice",
--       "friendly"
--     ]

>>>>>>> 2e4246c0a603aa179f49874443d1b6a62b208d30
-- newPerson :: IO Person
-- newPerson = do
--   nameI <- randomRIO (0, length nameTree - 1)
--   adjI <- randomRIO (0, length adjTree - 1)
--   durI <- randomRIO (0, length timeTree - 1)
--   time <- randomRIO (2, 9)
--   let name = nameTree !! nameI
--       adj = adjTree !! adjI
--       dur = durTree !! durI
--    in return
--         { name = nameTree !! nameI,
--           adj = adjTree !! adjI,
--           memberFor = show time ++ " " ++ durTree !! durI
--         }

-- addPerson :: Bool -> FingerTree Person -> FingerTree Person
-- addPerson fastpass ft = do
--   p <- newPerson
--   if fastpass then insertHead p ft else insertTail p ft

-- init :: Int -> FingerTree Person
-- init n = FT.fromList $ iterate (addPerson False) Nil !! n

-- complain :: Person -> String
-- complain p =
--   name p ++ " has been a " ++ adj p ++ " customer of Not-Disney for "
--     ++ time
--     ++ " "
--     ++ dur
--     ++ ", but that's going to change if they can't speak \
--        \to a manager RIGHT NOW."

-- getInt = do
--   input <- getLine
--   case (readMaybe input :: Maybe Int) of
--     Just i -> i
--     Nothing -> getInt

-- queueLoop :: FingerTree Person -> IO ()
-- queueLoop q =
--   let len = length q
--    in do
--         putStrLn "There are currently " ++ show len ++ " people in the queue."
--         angryI <- randomRIO (0, length q - 1)
--         putStrLn . complain . q !! angryI
--         putStrLn "Please type your response to this well-behaved customer."
--         input <- getLine
--         putStrLn
--           "After hearing your soothing words, the customer heads back to their\
--           \ spot in line."
--         putStrLn "How many people would you like to let into the ride?"
--         let admit = getInt
--         -- get input, bound it by length, let that many people in.
--         -- add 1-10 people to line
--         queueLoop

main :: IO ()
main = do
  putStrLn
    "Welcome to Not-Disney Not-Land-Or-World! How many people have \
    \rushed into the renowned Cosmos-Themed Landform Ride as soon as it opened?"

-- let queue = init getInt
