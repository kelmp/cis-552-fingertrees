{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module FingerTree 
  ( FingerTree (..),
    Some (..),
    Tuple (..),
    Split (..),
    split,
    splitTree,
    splitSome,
    splitTuple,
    (!!),
    more,
    pair,
    triple,
    Measured,
    measure,
    insertHead,
    insertTail,
    FingerTree.head,
    FingerTree.tail,
    isEmpty,
    append,
    toList,
    fromList,
    removeLast,
    FingerTree.concat,
    FingerTree.last) where

import Control.Applicative ()
import Control.Monad
import Data.Foldable ()
import Data.Functor ()
import Data.Monoid ()
import Data.Semigroup ()
import Data.Traversable ()
import Test.HUnit ()
import Test.QuickCheck
import Prelude hiding ((!!))

default (Int)

----------------
-- DATA TYPES --
----------------
----------------
-- FingerTree --
----------------
-- FingerTree is empty, a single element, or has elements on both sides
-- w/ a Tuple FingerTree in the middle. Nested type has cached length
data FingerTree a
  = Nil
  | Unit a
  | More Int (Some a) (FingerTree (Tuple a)) (Some a)
  deriving (Eq, Show)

-- Outside of a FingerTree. No length cached, since they are only the outermost
-- type in the first layer of the tree
data Some a
  = One a
  | Two a a
  | Three a a a
  deriving (Eq, Show)

-- Used in nested FingerTrees. Length is cached since they contain some unknown
-- number of elements in lower layers
data Tuple a
  = Pair Int a a
  | Triple Int a a a
  deriving (Eq, Show)

--------------
-- Measured --
--------------
-- Allows for O(1) retrieval of length via cached values
class Measured a where
  measure :: a -> Int
  default measure :: a -> Int
  measure _ = 1

instance Measured Int

instance Measured a => Measured (FingerTree a) where
  measure Nil = 0
  measure (Unit x) = measure x
  measure (More l _ _ _) = l

instance Measured a => Measured (Some a) where
  measure (One x) = measure x
  measure (Two x y) = measure x + measure y
  measure (Three x y z) = measure x + measure y + measure z

instance Measured (Tuple a) where
  measure (Pair l _ _) = l
  measure (Triple l _ _ _) = l

----------------------
-- Queue operations --
----------------------
insertHead :: Measured a => a -> FingerTree a -> FingerTree a
insertHead a Nil = Unit a
insertHead a (Unit b) = more (One a) Nil (One b)
insertHead a (More _ (One b) ft r) = more (Two a b) ft r
insertHead a (More _ (Two b c) ft r) = more (Three a b c) ft r
insertHead a (More _ (Three b c d) ft r) =
  more (Two a b) (insertHead (pair c d) ft) r

insertTail :: Measured a => a -> FingerTree a -> FingerTree a
insertTail z Nil = Unit z
insertTail z (Unit a) = more (One a) Nil (One z)
insertTail z (More _ l ft (One a)) = more l ft (Two a z)
insertTail z (More _ l ft (Two a b)) = more l ft (Three a b z)
insertTail z (More _ l ft (Three a b c)) =
  more l (insertTail (pair a b) ft) (Two c z)

head :: FingerTree a -> Maybe a
head Nil = Nothing
head (Unit x) = Just x
head (More _ (One x) _ _) = Just x
head (More _ (Two x _) _ _) = Just x
head (More _ (Three x _ _) _ _) = Just x

last :: FingerTree a -> Maybe a
last Nil = Nothing
last (Unit x) = Just x
last (More _ _ _ (One x)) = Just x
last (More _ _ _ (Two _ x)) = Just x
last (More _ _ _ (Three _ _ x)) = Just x

------------------------
-- Smart constructors --
------------------------
more :: Measured a => Some a -> FingerTree (Tuple a) -> Some a -> FingerTree a
more l ft r = More (measure l + measure ft + measure r) l ft r

pair :: Measured a => a -> a -> Tuple a
pair a b = Pair (measure a + measure b) a b

triple :: Measured a => a -> a -> a -> Tuple a
triple a b c = Triple (measure a + measure b + measure c) a b c

-----------
-- Split --
-----------
-- Type returned by the split function and some of its helpers.
data Split o a = Split o a o
  deriving (Eq, Show)

(!!) :: Measured a => FingerTree a -> Int -> Maybe a
ft !! i = case snd $ split i ft of
  Nil -> Nothing
  Unit x -> Just x
  More _ l _ _ -> Just $ snd $ someSeparateLast l

-- Splits a FingerTree into two separate trees based on index.
split :: Measured a => Int -> FingerTree a -> (FingerTree a, FingerTree a)
split _ Nil = (Nil, Nil)
split i ft =
  if i >= 0 && i < measure ft
    then
      let Split l x r = splitTree i ft
       in (l, insertHead x r)
    else (ft, Nil)

-- Split a tree, if possible. Because of the Split datatype requiring an
-- element in the middle, it's not possible for Nil but will work in all
-- other cases. This could be combined with "split" to avoid the error case,
-- but in the interest of sticking to the paper, it's like this for now.
-- This is a helper for split.
splitTree :: Measured a => Int -> FingerTree a -> Split (FingerTree a) a
splitTree _ Nil =
  error
    "Cannot call splitTree on empty FingerTree due to \
    \limitations of Split datatype"
splitTree _ (Unit x) = Split Nil x Nil
splitTree tgtI (More _ l ft r)
  | tgtI < startMiddle =
    let Split sl sx sr = splitSome tgtI l
     in Split (maybe Nil someToTree sl) sx (deepL sr ft r)
  | tgtI < startRight =
    let Split ml x' mr = splitTree (tgtI - measure l) ft
        Split il x'' ir = splitTuple (tgtI - measure l - measure ml) x'
     in Split (deepR l ml il) x'' (deepL ir mr r)
  | otherwise =
    let Split sl sx sr = splitSome (tgtI - measure l - measure ft) r
     in Split (deepR l ft sl) sx (maybe Nil someToTree sr)
  where
    startMiddle = measure l
    startRight = startMiddle + measure ft

-- Splits a Some based on relative index. Returns Split with Some on the
-- outsides (wrapped in Maybe in case there aren't enough elements)
-- to build the outsides of FingerTrees post-split.
-- This is a helper for splitTree
splitSome :: Measured a => Int -> Some a -> Split (Maybe (Some a)) a
splitSome _ (One x) = Split Nothing x Nothing
splitSome tgtI (Two x y) = splitTwoOrPair tgtI x y
splitSome tgtI (Three x y z) = splitThreeOrTriple tgtI x y z

-- Splits a Tuple based on relative index. Returns Some on the outside
-- of the Split type (if possible) since the results are used for the
-- outer branches of new More type FingerTrees.
-- This is a helper for splitTree.
splitTuple :: Measured a => Int -> Tuple a -> Split (Maybe (Some a)) a
splitTuple tgtI (Pair _ x y) = splitTwoOrPair tgtI x y
splitTuple tgtI (Triple _ x y z) = splitThreeOrTriple tgtI x y z

splitTwoOrPair :: Measured a => Int -> a -> a -> Split (Maybe (Some a)) a
splitTwoOrPair tgtI x y
  | tgtI < measure x = Split Nothing x (Just (One y)) -- ((), x y)
  | otherwise = Split (Just (One x)) y Nothing        -- (x, y)

splitThreeOrTriple :: Measured a =>
  Int -> a -> a -> a -> Split (Maybe (Some a)) a
splitThreeOrTriple tgtI x y z
  | tgtI < measure x = Split Nothing x (Just (Two y z)) -- ((), x y z)
  | tgtI < measure x + measure y =
      Split (Just (One x)) y (Just (One z))             -- (x, y z)
  | otherwise = Split (Just (Two x y)) z Nothing        -- (x y, z)

-- split helpers
-- splitTree needs deepL, deepR, and someToTree
deepL :: Measured a => Maybe (Some a) -> FingerTree (Tuple a) -> Some a ->
  FingerTree a
deepL Nothing ft r = rotL ft r
deepL (Just l) ft r = more l ft r

deepR :: Measured a => Some a -> FingerTree (Tuple a) -> Maybe (Some a) ->
  FingerTree a
deepR l ft Nothing = rotR l ft
deepR l ft (Just r) = more l ft r

someToTree :: Measured a => Some a -> FingerTree a
someToTree (One a) = Unit a
someToTree (Two a b) = more (One a) Nil (One b)
someToTree (Three a b c) = more (One a) Nil (Two b c)

-- deepL and deepR need rotL/rotR helpers, and a smart constructor for More

-- Inputs represent a FingerTree with missing left branch; shift middle to
-- left by 1 (if possible) to construct valid FingerTree
rotL :: Measured a => FingerTree (Tuple a) -> Some a -> FingerTree a
rotL ft r = case ft of
  Nil -> someToTree r
  Unit x -> more (tupleToSome x) Nil r
  -- More case needs to lift left el of left branch of inner FT out of its tuple
  More _ il ft' ir -> case someSeparateHead il of
    (h, Nothing) -> more (tupleToSome h) (rotL ft' ir) r
    (h, Just t) -> more (tupleToSome h) (more t ft' ir) r

-- Similar to rotL, but there's no right branch (so rotate to right by 1)
rotR :: Measured a => Some a -> FingerTree (Tuple a) -> FingerTree a
rotR l ft = case ft of
  Nil -> someToTree l
  Unit x -> more l Nil (tupleToSome x)
  -- lifting right branch of inner FT out of tuple
  More _ il ft' ir -> case someSeparateLast ir of
    (Nothing, last) -> more l (rotR il ft') (tupleToSome last)
    (Just first, last) -> more l (more il ft' first) (tupleToSome last)

-- helper for rotL. The head becomes the missing left branch of the overall FT,
-- and the remainder becomes the left branch of the inner FT
someSeparateHead :: Some a -> (a, Maybe (Some a))
someSeparateHead (One x) = (x, Nothing)
someSeparateHead (Two x y) = (x, Just (One y))
someSeparateHead (Three x y z) = (x, Just (Two y z))

-- helper for rotL; the first (len-1) elements become the right branch of the
-- inner FT, and the last el becomes the missing right branch of the overall FT
someSeparateLast :: Some a -> (Maybe (Some a), a)
someSeparateLast (One x) = (Nothing, x)
someSeparateLast (Two x y) = (Just (One x), y)
someSeparateLast (Three x y z) = (Just (Two x y), z)

tupleToSome :: Tuple a -> Some a
tupleToSome (Pair _ a b) = Two a b
tupleToSome (Triple _ a b c) = Three a b c

-- instance Monad FingerTree where
--   return :: a -> FingerTree a
--   return = Unit

--   (>>=) :: FingerTree a -> (a -> FingerTree b) -> FingerTree b
--   Nil >>= f = Nil
--   (Unit x) >>= f = f x
--   (More n l t r) >>= f = undefined

-- instead of making it an instance of functor, we just make our own fmap
-- (Or, we could potentially write a function that measures a non-meaured type)

fmap' :: Measured a => Measured b => (a -> b) -> FingerTree a -> FingerTree b
fmap' _ Nil = Nil
fmap' f (Unit x) = Unit (f x)
fmap' f (More _ l t r) = more (fmapSome f l) (fmap' (fmapTuple f) t) (fmapSome f r)

fmapSome :: Measured a => Measured b => (a -> b) -> Some a -> Some b
fmapSome f (One x) = One (f x)
fmapSome f (Two x y) = Two (f x) (f y)
fmapSome f (Three x y z) = Three (f x) (f y) (f z)

fmapTuple :: Measured a => Measured b => (a -> b) -> Tuple a -> Tuple b
fmapTuple f (Pair _ x y) = pair (f x) (f y)
fmapTuple f (Triple _ x y z) = triple (f x) (f y) (f z)

-- fmap is supposed to take in two measured things but, it can't

-- toMeasured :: Measured c => b -> c
-- toMeasured x = Measured x

mapSome :: Some a -> (a -> b) -> Some b
mapSome (One x) f = One (f x)
mapSome (Two x y) f = Two (f x) (f y)
mapSome (Three x y z) f = Three (f x) (f y) (f z)

-- instance Applicative FingerTree where
--   pure = undefined
--   (<*>) = undefined

-- instance Applicative FingerTree where
--   pure :: a -> FingerTree a
--   pure = Unit

--   (<*>) :: FingerTree (a -> b) -> FingerTree a -> FingerTree b
--   Nil <*> _ = Nil
-- 	_ <*> Nil = Nil
-- 	(Unit f) <*> (Unit x) = Unit (f x)
--   (Unit f) <*> (More _ _ x _) = Unit (f x)
-- 	(Node _ f _) <*> (Unit v) = undefined
-- 	(Node l f r) <*> (Node l' v r') =
-- 		Node (l <*> l') (f v) (r <*> r')

instance Measured a => Monoid (FingerTree a) where
  mempty :: FingerTree a
  mempty = Nil

instance Measured a => Semigroup (FingerTree a) where
  (<>) :: Measured a => FingerTree a -> FingerTree a -> FingerTree a
  (<>) = append

instance Foldable FingerTree where
  foldMap :: Monoid m => (a -> m) -> FingerTree a -> m
  foldMap f Nil = mempty
  foldMap f (Unit x) = f x
  foldMap f (More n l t r) = foldMap f l `mappend` foldMap (foldMap f) t `mappend` foldMap f r

instance Foldable Tuple where
  foldMap :: Monoid m => (a -> m) -> Tuple a -> m
  foldMap f (Pair _ x y) = f x `mappend` f y
  foldMap f (Triple _ x y z) = f x `mappend` f y `mappend` f z

instance Foldable Some where
  foldMap :: Monoid m => (a -> m) -> Some a -> m
  foldMap f (One x) = f x
  foldMap f (Two x y) = f x `mappend` f y
  foldMap f (Three x y z) = f x `mappend` f y `mappend` f z

-- instance Traversable FingerTree where
--   traverse :: Applicative z => (a -> z b) -> FingerTree a -> z (FingerTree b)
--   traverse f t = undefined -- TODO

------ Functions ------

-- TODO check i
tail :: Measured a => FingerTree a -> FingerTree a
tail (Unit _) = Nil
tail (More _ (Three _ x y) ft r) = more (Two x y) ft r
tail (More _ (Two _ x) ft r) = more (One x) ft r
tail (More _ (One _) ft r) = case FingerTree.head ft of
  -- ft is Nil
  Nothing -> someToTree r
  -- ft is not Nil
  Just (Pair _ x y) -> more (Two x y) (FingerTree.tail ft) r
  Just (Triple _ x _ _) -> more (One x) (map1 chop ft) r
    where chop (Triple _ _ y z) = pair y z

map1 :: Measured a => (a -> a) -> FingerTree a -> FingerTree a
map1 _ Nil = Nil
map1 f (Unit x) = Unit (f x)
map1 f (More _ (One x) ft r) = more (One (f x)) ft r
map1 f (More _ (Two x y) ft r) = more (Two (f x) y) ft r
map1 f (More _ (Three x y z) ft r) = more (Three (f x) y z) ft r

  -- (Nil, One x) -> Just $ Unit x
  -- (Nil, Two x y) -> Just $ more (One x) Nil (One y)
  -- (Nil, Three x y z) -> Just $ more (One x) Nil (Two y z)
  -- _ -> undefined

-- case FingerTree.head ft of
--   Just (Pair x y) -> Just $ More (Two x y) (FingerTree.tail ft) r

removeLast :: Measured a => FingerTree a -> FingerTree a
removeLast (Unit _) = Nil
removeLast (More _ l ft (Three x y _)) = more l ft (Two x y)
removeLast (More _ l ft (Two x _)) = more l ft (One x)
removeLast (More _ l ft (One _)) = case FingerTree.last ft of
  -- ft is Nil
  Nothing -> someToTree l
  -- ft is not Nil
  Just (Pair _ x y) -> more l (removeLast ft) (Two x y)
  Just (Triple _ x y z) -> more l (removeLast ft) (Three x y z)
  

isEmpty :: FingerTree a -> Bool
isEmpty t = case t of
  Nil -> True
  _ -> False

append :: Measured a => FingerTree a -> FingerTree a -> FingerTree a
append t1 = glue t1 []

glue :: Measured a => FingerTree a -> [a] -> FingerTree a -> FingerTree a
glue Nil l t2 = foldr insertHead t2 l
glue t1 l Nil = foldl (flip insertTail) t1 l
glue (Unit x) l t2 = foldr insertHead t2 (x : l)
glue t1 l (Unit y) = foldl (flip insertTail) t1 (l ++ [y])
glue (More i1 x1 t1 y1) l (More _ x2 t2 y2) =
  More i1 x1 (glue t1 (listToTuples (someToList y1 ++ l ++ someToList x2)) t2) y2

someToList :: Some a -> [a]
someToList (One x) = [x]
someToList (Two x y) = [x, y]
someToList (Three x y z) = [x, y, z]

listToTuples :: Measured a => [a] -> [Tuple a]
listToTuples [] = []
listToTuples [x, y] = [pair x y]
listToTuples [x, y, z, w] = [pair x y, pair z w]
listToTuples (x : y : z : xs) = triple x y z : listToTuples xs

concat :: [FingerTree a] -> FingerTree a
concat l = undefined

-- TODO: Figure out what to do if this contains tuples
toList :: FingerTree a -> [a]
toList Nil = []
toList (Unit x) = [x]
toList (More _ l ft r) = someToList l ++ foldr (\x acc -> tupleToList x ++ acc) [] (toList ft) ++ someToList r

fromList :: Measured a => [a] -> FingerTree a
fromList = foldr insertHead Nil

arbitrarySizedTree :: Measured a => Arbitrary a => Int -> Gen (FingerTree a)
arbitrarySizedTree m
  | m == 0 = return Nil
  | m > 100 = arbitrarySizedTree 100
  | otherwise = liftM2 insertHead arbitrary (arbitrarySizedTree (m - 1))
    -- liftM2 (<>) (arbitrarySizedTree (m - 1)) (Unit <$> arbitrary)

instance (Show a, Arbitrary a, Measured a) => Arbitrary (FingerTree a) where
  arbitrary = sized arbitrarySizedTree

  shrink :: Measured a => FingerTree a -> [FingerTree a]
  shrink Nil = []
  shrink (Unit _) = [Nil]
  shrink (More _ l t r) =
    someToTreeList l ++ someToTreeList r ++ shrink (tupleTreeToTree t)
      ++ [Nil]

tupleTreeToTree :: Measured a => FingerTree (Tuple a) -> FingerTree a
tupleTreeToTree Nil = Nil
tupleTreeToTree (Unit (Pair _ x y)) = More 2 (One x) Nil (One y)
tupleTreeToTree (Unit (Triple _ x y z)) = More 2 (Two x y) Nil (One z)
tupleTreeToTree (More _ l Nil r) = fromList (someTupleToList l ++ someTupleToList r)
tupleTreeToTree (More n left ft right) = case (left, right) of
  (One l, One r) -> More n (tupleToSome l) (tupleTreeToTree ft) (tupleToSome r)
  (One l, Two r1 r2) -> More n (tupleToSome l) (insertTail r1 (tupleTreeToTree ft)) (tupleToSome r2)
  (One l, Three r1 r2 r3) -> More n (tupleToSome l) (insertTail r2 (insertTail r1 (tupleTreeToTree ft))) (tupleToSome r3)
  (Two l1 l2, One r) -> More n (tupleToSome l1) (insertHead l2 (tupleTreeToTree ft)) (tupleToSome r)
  (Two l1 l2, Two r1 r2) -> More n (tupleToSome l1) (insertTail r1 (insertHead l2 (tupleTreeToTree ft))) (tupleToSome r2)
  (Two l1 l2, Three r1 r2 r3) -> More n (tupleToSome l1) (insertTail r1 (insertTail r2 (insertHead l2 (tupleTreeToTree ft)))) (tupleToSome r3)
  (Three l1 l2 l3, One r) -> More n (tupleToSome l1) (insertHead l2 (insertHead l3 (tupleTreeToTree ft))) (tupleToSome r)
  (Three l1 l2 l3, Two r1 r2) -> More n (tupleToSome l1) (insertTail r1 (insertHead l2 (insertHead l3 (tupleTreeToTree ft)))) (tupleToSome r2)
  (Three l1 l2 l3, Three r1 r2 r3) -> More n (tupleToSome l1) (insertTail r1 (insertTail r2 (insertHead l2 (insertHead l3 (tupleTreeToTree ft))))) (tupleToSome r3)

someTupleToList :: Some (Tuple a) -> [a]
someTupleToList (One t) = tupleToList t
someTupleToList (Two t1 t2) = tupleToList t1 ++ tupleToList t2
someTupleToList (Three t1 t2 t3) = tupleToList t1 ++ tupleToList t2 ++ tupleToList t3

tupleToList :: Tuple a -> [a]
tupleToList (Pair _ x y) = [x, y]
tupleToList (Triple _ x y z) = [x, y, z]

someToTreeList :: Some a -> [FingerTree a]
someToTreeList (One x) = [Unit x]
someToTreeList (Two x y) = [Unit x, Unit y, More 2 (One x) Nil (One y)]

-- ft1 :: FingerTree Int
-- ft1 = Unit 1

-- ft2 :: FingerTree Int
-- ft2 = More 2 (One 1) Nil (One 2)

-- ft3 :: FingerTree Int
-- ft3 = More 3 (One 1) Nil (Two 2 3)

-- ft4 :: FingerTree Int
-- ft4 = More 4 (One 1) Nil (Three 2 3 4)