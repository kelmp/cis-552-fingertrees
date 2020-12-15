{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FingerTree2
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
    FingerTree2.head,
    FingerTree2.tail,
    isEmpty,
    append,
    toList,
    fromList,
    removeLast,
    FingerTree2.last,
    fmap',
    arbitrary,
    shrink,
  )
where

import Control.Applicative ()
import Control.DeepSeq (NFData)
import Control.Monad (Monad (return), liftM2)
import Data.Foldable ()
import Data.Functor ()
import Data.Monoid ()
import Data.Semigroup ()
import Data.Traversable ()
import GHC.Generics (Generic)
import Test.QuickCheck
import Prelude hiding (head, last, tail)

----------------
-- DATA TYPES --
----------------
----------------
-- FingerTree --
----------------
-- FingerTree is empty, a single element, or has elements on both sides
-- w/ a Tuple FingerTree in the middle. Nested type has cached length
data FingerTree c a
  = Nil
  | Unit a
  | More c (Some a) (FingerTree c (Tuple c a)) (Some a)
  deriving (Eq, Show, Generic, NFData)

-- Outside of a FingerTree. No length cached, since they are only contain the
-- base type in the outer branches of the first layer of the tree
data Some a
  = One a
  | Two a a
  | Three a a a
  deriving (Eq, Show, Generic, NFData)

-- Used in nested FingerTrees. Length is cached since they contain some unknown
-- number of elements in lower layers
data Tuple c a
  = Pair c a a
  | Triple c a a a
  deriving (Eq, Show, Generic, NFData)

--------------
-- Measured --
--------------
-- Allows for O(1) retrieval of length via cached values
class (Monoid c) => Measured c a where
  measure :: a -> c
  -- Let arbitrary stored types be measured by assuming any contained value
  -- has measurement 1
  default measure :: Num c => a -> c
  measure _ = 1

instance Measured c a => Measured c (FingerTree c a) where
  measure Nil = mempty
  measure (Unit x) = measure x
  measure (More l _ _ _) = l

instance Measured c a => Measured c (Some a) where
  measure (One x) = measure x
  measure (Two x y) = measure x <> measure y
  measure (Three x y z) = measure x <> measure y <> measure z

instance Measured c a => Measured c (Tuple c a) where
  measure (Pair l _ _) = l
  measure (Triple l _ _ _) = l

instance (Measured c a) => Semigroup (FingerTree c a) where
  (<>) = append

instance (Measured c a) => Monoid (FingerTree c a) where
  mempty = Nil
  mappend = append

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0
  mappend = (+)

instance Measured Int Int

------------------------
-- Smart constructors --
------------------------
more ::
  Measured c a =>
  Some a ->
  FingerTree c (Tuple c a) ->
  Some a ->
  FingerTree c a
more l ft r = More (measure l <> measure ft <> measure r) l ft r

pair :: Measured c a => a -> a -> Tuple c a
pair x y = Pair (measure x <> measure y) x y

triple :: Measured c a => a -> a -> a -> Tuple c a
triple x y z = Triple (measure x <> measure y <> measure z) x y z

------------------
-- Foldable --
------------------
instance Foldable (FingerTree c) where
  foldMap :: Monoid m => (a -> m) -> FingerTree c a -> m
  foldMap _ Nil = mempty
  foldMap f (Unit x) = f x
  foldMap f (More _ l t r) = foldMap f l <> foldMap (foldMap f) t <> foldMap f r

instance Foldable (Tuple c) where
  foldMap :: Monoid m => (a -> m) -> Tuple c a -> m
  foldMap f (Pair _ x y) = f x <> f y
  foldMap f (Triple _ x y z) = f x <> f y <> f z

instance Foldable Some where
  foldMap :: Monoid m => (a -> m) -> Some a -> m
  foldMap f (One x) = f x
  foldMap f (Two x y) = f x <> f y
  foldMap f (Three x y z) = f x <> f y <> f z

------------------------
-- Pseudo-applicative --
------------------------
fmap' ::
  (Measured c1 a, Measured c2 b) =>
  (a -> b) ->
  FingerTree c1 a ->
  FingerTree c2 b
fmap' _ Nil = Nil
fmap' f (Unit x) = Unit (f x)
fmap' f (More _ l t r) =
  more (fmapSome f l) (fmap' (fmapTuple f) t) (fmapSome f r)

fmapSome :: (a -> b) -> Some a -> Some b
fmapSome f (One x) = One (f x)
fmapSome f (Two x y) = Two (f x) (f y)
fmapSome f (Three x y z) = Three (f x) (f y) (f z)

fmapTuple ::
  (Measured c1 a, Measured c2 b) =>
  (a -> b) ->
  Tuple c1 a ->
  Tuple c2 b
fmapTuple f (Pair _ x y) = pair (f x) (f y)
fmapTuple f (Triple _ x y z) = triple (f x) (f y) (f z)

-----------
-- Split --
-----------
data Split o a = Split o a o
  deriving (Eq, Show)

-- Splits a FingerTree into two separate trees.
-- Monotonicity of predicate is encouraged - otherwise, this will find *a* flip
-- if the predicate holds at the end, but not necessarily the first.
split ::
  Measured c a =>
  (c -> Bool) ->
  FingerTree c a ->
  (FingerTree c a, FingerTree c a)
split _ Nil = (Nil, Nil)
split pred ft =
  if pred $ measure ft -- predicate is true at the end of tree
    then
      let Split l x r = splitTree pred mempty ft
       in (l, insertHead x r)
    else (ft, Nil)

-- Split a tree, if possible. Because of the Split datatype requiring an
-- element in the middle, it's not possible for Nil but will work in all
-- other cases.
splitTree ::
  Measured c a =>
  (c -> Bool) ->
  c ->
  FingerTree c a ->
  Split (FingerTree c a) a
splitTree _ _ Nil =
  error
    "Cannot call splitTree on empty FingerTree due to \
    \limitations of Split datatype"
splitTree _ _ (Unit x) = Split Nil x Nil
splitTree pred curr (More _ l ft r)
  | pred startMiddle -- if start of middle is true, flip could be in left
    =
    let Split sl sx sr = splitSome pred curr l
     in Split (maybe Nil someToTree sl) sx (deepL sr ft r)
  | pred startRight -- if start of right is true, flip could be in middle
    =
    let Split ml x' mr = splitTree pred startMiddle ft
        Split il x'' ir = splitTuple pred curr x'
     in Split (deepR l ml il) x'' (deepL ir mr r)
  | otherwise =
    let Split sl sx sr = splitSome pred startRight r
     in Split (deepR l ft sl) sx (maybe Nil someToTree sr)
  where
    startMiddle = measure l
    startRight = startMiddle <> measure ft

-- Split a Some type based on a predicate on cached value, and the current
-- cached value (updated by prior helpers of "split")
splitSome ::
  (Measured c a) =>
  (c -> Bool) ->
  c ->
  Some a ->
  Split (Maybe (Some a)) a
splitSome pred curr (One x) = Split Nothing x Nothing
splitSome pred curr (Two x y) = splitTwoOrPair pred curr x y
splitSome pred curr (Three x y z) = splitThreeOrTriple pred curr x y z

-- Same as splitSome, but for splitting tuples
splitTuple ::
  (Measured c a) =>
  (c -> Bool) ->
  c ->
  Tuple c a ->
  Split (Maybe (Some a)) a
splitTuple pred curr (Pair _ x y) = splitTwoOrPair pred curr x y
splitTuple pred curr (Triple _ x y z) = splitThreeOrTriple pred curr x y z

-- Splitting Some's Two and Tuple's Pair follows the same logic.
-- If pred holds in the first element, both elements are post-flip.
-- If pred only holds in the second element, only the second is post-flip.
splitTwoOrPair ::
  (Measured c a) =>
  (c -> Bool) ->
  c ->
  a ->
  a ->
  Split (Maybe (Some a)) a
splitTwoOrPair pred curr x y
  | pred (curr <> measure x) = Split Nothing x (Just (One y)) -- ((), x y)
  | otherwise = Split (Just (One x)) y Nothing -- (x, y)

-- Similar to splitTwoOrPair, but for Some's Three and Tuple's Triple
splitThreeOrTriple ::
  (Measured c a) =>
  (c -> Bool) ->
  c ->
  a ->
  a ->
  a ->
  Split (Maybe (Some a)) a
splitThreeOrTriple pred curr x y z
  | pred startY = Split Nothing x (Just (Two y z)) -- ((), x y z)
  | pred startZ = Split (Just (One x)) y (Just (One z)) -- (x, y z)
  | otherwise = Split (Just (Two x y)) z Nothing -- (x y, z)
  where
    startY = curr <> measure x
    startZ = startY <> measure y

-- split helpers
-- splitTree needs deepL, deepR, and someToTree
deepL ::
  Measured c a =>
  Maybe (Some a) ->
  FingerTree c (Tuple c a) ->
  Some a ->
  FingerTree c a
deepL Nothing ft r = rotL ft r
deepL (Just l) ft r = more l ft r

deepR ::
  Measured c a =>
  Some a ->
  FingerTree c (Tuple c a) ->
  Maybe (Some a) ->
  FingerTree c a
deepR l ft Nothing = rotR l ft
deepR l ft (Just r) = more l ft r

someToTree :: Measured c a => Some a -> FingerTree c a
someToTree (One a) = Unit a
someToTree (Two a b) = more (One a) Nil (One b)
someToTree (Three a b c) = more (One a) Nil (Two b c)

-- deepL and deepR need rotL/rotR helpers, and a smart constructor for More

-- Inputs represent a FingerTree with missing left branch; shift middle to
-- left by 1 (if possible) to construct valid FingerTree
rotL :: Measured c a => FingerTree c (Tuple c a) -> Some a -> FingerTree c a
rotL ft r = case ft of
  Nil -> someToTree r
  Unit x -> more (tupleToSome x) Nil r
  -- More case needs to lift left el of left branch of inner FT out of its tuple
  More _ il ft' ir -> case someSeparateHead il of
    (h, Nothing) -> more (tupleToSome h) (rotL ft' ir) r
    (h, Just t) -> more (tupleToSome h) (more t ft' ir) r

-- Similar to rotL, but there's no right branch (so rotate to right by 1)
rotR :: Measured c a => Some a -> FingerTree c (Tuple c a) -> FingerTree c a
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

tupleToSome :: Tuple c a -> Some a
tupleToSome (Pair _ a b) = Two a b
tupleToSome (Triple _ a b c) = Three a b c

---------------
-- Insertion --
---------------
-- Insertion to head and tail are both O(1)
insertHead :: Measured c a => a -> FingerTree c a -> FingerTree c a
insertHead a Nil = Unit a
insertHead a (Unit b) = more (One a) Nil (One b)
insertHead a (More _ (One b) ft r) = more (Two a b) ft r
insertHead a (More _ (Two b c) ft r) = more (Three a b c) ft r
insertHead a (More _ (Three b c d) ft r) =
  more (Two a b) (insertHead (pair c d) ft) r

insertTail :: Measured c a => a -> FingerTree c a -> FingerTree c a
insertTail z Nil = Unit z
insertTail z (Unit a) = more (One a) Nil (One z)
insertTail z (More _ l ft (One a)) = more l ft (Two a z)
insertTail z (More _ l ft (Two a b)) = more l ft (Three a b z)
insertTail z (More _ l ft (Three a b c)) =
  more l (insertTail (pair a b) ft) (Two c z)

------------
-- Append --
------------
append :: Measured c a => FingerTree c a -> FingerTree c a -> FingerTree c a
append t1 = glue t1 []

-- Append helpers
glue ::
  Measured c a =>
  FingerTree c a ->
  [a] ->
  FingerTree c a ->
  FingerTree c a
glue Nil l t2 = foldr insertHead t2 l
glue t1 l Nil = foldl (flip insertTail) t1 l
glue (Unit x) l t2 = foldr insertHead t2 (x : l)
glue t1 l (Unit y) = foldl (flip insertTail) t1 (l ++ [y])
glue (More i1 x1 t1 y1) l (More i2 x2 t2 y2) =
  More
    i1
    x1
    (glue t1 (listToTuples (someToList y1 ++ l ++ someToList x2)) t2)
    y2

someToList :: Some a -> [a]
someToList (One x) = [x]
someToList (Two x y) = [x, y]
someToList (Three x y z) = [x, y, z]

listToTuples :: Measured c a => [a] -> [Tuple c a]
listToTuples [] = []
listToTuples [x, y] = [pair x y]
listToTuples [x, y, z, w] = [pair x y, pair z w]
listToTuples (x : y : z : xs) = triple x y z : listToTuples xs

----------------------------------
-- Head/tail related operations --
----------------------------------
-- first element
head :: FingerTree c a -> Maybe a
head Nil = Nothing
head (Unit x) = Just x
head (More _ (One x) _ _) = Just x
head (More _ (Two x _) _ _) = Just x
head (More _ (Three x _ _) _ _) = Just x

-- last element
last :: FingerTree c a -> Maybe a
last Nil = Nothing
last (Unit x) = Just x
last (More _ _ _ (One x)) = Just x
last (More _ _ _ (Two _ x)) = Just x
last (More _ _ _ (Three _ _ x)) = Just x

-- everything but the head
tail :: Measured c a => FingerTree c a -> FingerTree c a
tail (More _ (Three _ x y) ft r) = more (Two x y) ft r
tail (More _ (Two _ x) ft r) = more (One x) ft r
tail (More _ (One _) ft r) = case head ft of
  -- ft is Nil
  Nothing -> someToTree r
  -- ft is not Nil
  Just (Pair _ x y) -> more (Two x y) (tail ft) r
  Just (Triple _ x _ _) -> more (One x) (map1 chop ft) r
    where
      chop (Triple _ _ y z) = pair y z
tail _ = Nil

-- helper for tail
map1 :: Measured c a => (a -> a) -> FingerTree c a -> FingerTree c a
map1 _ Nil = Nil
map1 f (Unit x) = Unit (f x)
map1 f (More _ (One x) ft r) = more (One (f x)) ft r
map1 f (More _ (Two x y) ft r) = more (Two (f x) y) ft r
map1 f (More _ (Three x y z) ft r) = more (Three (f x) y z) ft r

-- everything but the last element
removeLast :: Measured c a => FingerTree c a -> FingerTree c a
removeLast (More _ l ft (Three x y _)) = more l ft (Two x y)
removeLast (More _ l ft (Two x _)) = more l ft (One x)
removeLast (More _ l ft (One _)) = case last ft of
  -- ft is Nil
  Nothing -> someToTree l
  -- ft is not Nil
  Just (Pair _ x y) -> more l (removeLast ft) (Two x y)
  Just (Triple _ x y z) -> more l (removeLast ft) (Three x y z)
removeLast _ = Nil

---------------------
-- Other functions --
---------------------

fromList :: Measured c a => c -> [a] -> FingerTree c a
fromList mes = foldr insertHead (Nil :: FingerTree mes a)

-- this was only used for testing
isEmpty :: FingerTree c a -> Bool
isEmpty Nil = True
isEmpty _ = False

toList :: FingerTree c a -> [a]
toList Nil = []
toList (Unit x) = [x]
toList (More _ l ft r) =
  someToList l
    ++ foldr (\x acc -> tupleToList x ++ acc) [] (toList ft)
    ++ someToList r

tupleToList :: Tuple c a -> [a]
tupleToList (Pair _ x y) = [x, y]
tupleToList (Triple _ x y z) = [x, y, z]

arbitrarySizedTree :: Measured c a => Arbitrary a => Int -> Gen (FingerTree c a)
arbitrarySizedTree m
  | m == 0 = return Nil
  | m > 100 = arbitrarySizedTree 100
  | otherwise = liftM2 insertHead arbitrary (arbitrarySizedTree (m - 1))

instance (Show a, Arbitrary a, Measured c a) => Arbitrary (FingerTree c a) where
  arbitrary = sized arbitrarySizedTree

  shrink :: Measured c a => FingerTree c a -> [FingerTree c a]
  shrink Nil = []
  shrink (Unit _) = [Nil]
  shrink (More m l t r) =
    someToTreeList m l ++ someToTreeList m r ++ shrink (tupleTreeToTree t)
      ++ [Nil]

tupleTreeToTree :: Measured c a => FingerTree c (Tuple c a) -> FingerTree c a
tupleTreeToTree Nil = Nil
tupleTreeToTree (Unit (Pair m x y)) = More m (One x) Nil (One y)
tupleTreeToTree (Unit (Triple m x y z)) = More m (Two x y) Nil (One z)
tupleTreeToTree (More m l Nil r) =
  fromList m (someTupleToList l ++ someTupleToList r)
tupleTreeToTree (More n left ft right) = case (left, right) of
  (One l, One r) -> More n (tupleToSome l) (tupleTreeToTree ft) (tupleToSome r)
  (One l, Two r1 r2) ->
    More n (tupleToSome l) (insertTail r1 (tupleTreeToTree ft)) (tupleToSome r2)
  (One l, Three r1 r2 r3) ->
    More
      n
      (tupleToSome l)
      (insertTail r2 (insertTail r1 (tupleTreeToTree ft)))
      (tupleToSome r3)
  (Two l1 l2, One r) ->
    More
      n
      (tupleToSome l1)
      (insertHead l2 (tupleTreeToTree ft))
      (tupleToSome r)
  (Two l1 l2, Two r1 r2) ->
    More
      n
      (tupleToSome l1)
      (insertTail r1 (insertHead l2 (tupleTreeToTree ft)))
      (tupleToSome r2)
  (Two l1 l2, Three r1 r2 r3) ->
    More
      n
      (tupleToSome l1)
      (insertTail r1 (insertTail r2 (insertHead l2 (tupleTreeToTree ft))))
      (tupleToSome r3)
  (Three l1 l2 l3, One r) ->
    More
      n
      (tupleToSome l1)
      (insertHead l2 (insertHead l3 (tupleTreeToTree ft)))
      (tupleToSome r)
  (Three l1 l2 l3, Two r1 r2) ->
    More
      n
      (tupleToSome l1)
      (insertTail r1 (insertHead l2 (insertHead l3 (tupleTreeToTree ft))))
      (tupleToSome r2)
  (Three l1 l2 l3, Three r1 r2 r3) ->
    More
      n
      (tupleToSome l1)
      ( insertTail
          r1
          ( insertTail
              r2
              ( insertHead
                  l2
                  (insertHead l3 (tupleTreeToTree ft))
              )
          )
      )
      (tupleToSome r3)

someTupleToList :: Some (Tuple c a) -> [a]
someTupleToList (One t) = tupleToList t
someTupleToList (Two t1 t2) = tupleToList t1 ++ tupleToList t2
someTupleToList (Three t1 t2 t3) =
  tupleToList t1 ++ tupleToList t2 ++ tupleToList t3

someToTreeList :: Measured c a => c -> Some a -> [FingerTree c a]
someToTreeList _ (One x) = [Unit x]
someToTreeList m (Two x y) = [Unit x, Unit y, More m (One x) Nil (One y)]
someToTreeList m (Three x y z) =
  someToTreeList m (Two x y)
    ++ someToTreeList m (Two y z)
    ++ someToTreeList m (Two x z)
    ++ [More m (Two x y) Nil (One z)]
    ++ [More m (Two x z) Nil (One y)]
    ++ [More m (Two y z) Nil (One x)]
    ++ [More m (Two y x) Nil (One z)]
    ++ [More m (Two z x) Nil (One y)]
    ++ [More m (Two z y) Nil (One x)]