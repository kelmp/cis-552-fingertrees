{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module OldFingerTree where

--   ( FingerTree (..),
--     insertHead,
--     insertTail,
--     FingerTree.head,
--     FingerTree.tail,
--     isEmpty,
--     append,
--     split,
--     toList,
--     fromList,
--     removeTail,
--     FingerTree.concat,
--     FingerTree.length,
--     FingerTree.last,
--   )

import Control.Applicative ()
import Control.Monad ()
import Data.Foldable ()
import Data.Functor ()
import Data.Monoid ()
import Data.Semigroup ()
import Data.Traversable ()
import Test.HUnit
import Test.QuickCheck

-- import Data.Traversable ()
-- import Test.HUnit
-- import Test.QuickCheck

data FingerTree a
  = Nil
  | Unit a
  | More Int (Some a) (FingerTree (Tuple a)) (Some a)
  deriving (Eq, Show)

data Some a
  = One a
  | Two a a
  | Three a a a
  deriving (Eq, Show)

data Tuple a
  = Pair Int a a
  | Triple Int a a a
  deriving (Eq, Show)

class Measured a where
  measure :: a -> Int

-- data FingerTree a
--   = Nil
--   | Unit Int a
--   | More Int (Some a) (FingerTree (Tuple a)) (Some a)
--   deriving (Eq, Show)

-- data Some a
--   = One Int a
--   | Two Int a a
--   | Three Int a a a
--   deriving (Eq, Show)

-- data Tuple a
--   = Pair Int a a
--   | Triple Int a a a
--   deriving (Eq, Show)

instance Measured (FingerTree a) where
  measure Nil = 0
  measure (Unit x) = measure x
  measure (More l _ _ _) = l

instance Measured (Some a) where
  measure One {} = 1
  measure Two {} = 2
  measure Three {} = 3

instance Measured (Tuple a) where
  measure (Pair l _ _) = l
  measure (Triple l _ _ _) = l

data Split o a = Split o a o
  deriving (Eq, Show)

-- splitSome :: Int -> Int -> Some a -> Split FingerTree a
-- splitSome _ _ (One x) = Split Nil x Nil
-- splitSome tgtI currI (Two x y)
--   | tgtI == currI + 1 = Split Nil x (Unit y)
--   | otherwise = Split (Unit x) y Nil
-- splitSome tgtI currI (Three x y z)
--   | tgtI == currI + 1 = Split Nil x (insertHead y (Unit z))
--   | tgtI == currI + 2 = Split (Unit x) y (Unit z)
--   | otherwise = Split (insertHead x (Unit y)) z Nil

-- ask whether there's a way to get nested types
-- splitSome :: Int -> Int -> Some a -> Split (Maybe (Digit a)) a
splitSome :: Int -> Some a -> Split (Maybe (Some a)) a
splitSome _ (One x) = Split Nothing x Nothing
splitSome tgtI (Two x y)
  | tgtI == 1 = Split Nothing x (Just (One y))
  | otherwise = Split (Just (One x)) y Nothing
splitSome tgtI (Three x y z)
  | tgtI == 1 = Split Nothing x (Just (Two y z))
  | tgtI == 2 = Split (Just (One x)) y (Just (One z))
  | otherwise = Split (Just (Two x y)) z Nothing

splitTuple :: Int -> Tuple a -> Split (Maybe (Some a)) a
splitTuple tgtI (Pair _ x y)
  | tgtI == 1 = Split Nothing x (Just (One y))
  | otherwise = Split (Just (One x)) y Nothing
splitTuple tgtI (Triple _ x y z)
  | tgtI == 1 = Split Nothing x (Just (Two y z))
  | tgtI == 2 = Split (Just (One x)) y (Just (One z))
  | otherwise = Split (Just (Two x y)) z Nothing

splitTree :: Int -> FingerTree a -> Split (FingerTree a) a
splitTree _ Nil = error "Cannot split empty FingerTree"
splitTree _ (Unit x) = Split Nil x Nil
splitTree tgtI (More len l ft r)
  | tgtI <= startMiddle =
    let Split sl sx sr = splitSome tgtI l
     in Split sl sx (deepL sr ft r)
  | tgtI <= startRight =
    let Split ml x' mr = splitTree (tgtI - measure l) ft
        Split il x'' ir = splitTuple (tgtI - measure l - measure ml) x'
     in Split (deepR l ml il) x'' (deepL ir mr r)
  | otherwise =
    let Split sl sx sr = splitSome (tgtI - measure l - measure ft) r
     in Split (deepR l ft sl) sx (someToTree sr)

split :: Int -> FingerTree a -> (FingerTree a, FingerTree a)
split _ Nil = (Nil, Nil)
split i ft =
  if i >= 0 && i < measure ft
    then
      let Split l x r = splitTree i ft
       in (l, insertHead x r)
    else (ft, Nil)

-- split helpers
-- splitTree needs deepL, deepR, and someToTree
deepL :: Maybe (Some a) -> FingerTree (Tuple a) -> Some a -> FingerTree a
deepL Nothing ft r = rotL ft r
deepL (Just l) ft r = more l ft r

deepR :: Some a -> FingerTree (Tuple a) -> Maybe (Some a) -> FingerTree a
deepR l ft Nothing = rotR l ft
deepR l ft (Just r) = more l ft r

someToTree :: Some a -> FingerTree a
someToTree (One a) = Unit a
someToTree (Two a b) = More (One a) Nil (One b)
someToTree (Three a b c) = More (One a) Nil (Two b c)

-- deepL and deepR need rotL/rotR helpers, and a smart constructor for More

-- Inputs represent a FingerTree with missing left branch; shift middle to
-- left by 1 (if possible) to construct valid FingerTree
rotL :: FingerTree (Tuple a) -> Some a -> FingerTree a
rotL ft r = case ft of
  Nil -> someToTree r
  Unit x -> more (tupleToSome x) Nil r
  -- More case needs to lift left el of left branch of inner FT out of its tuple
  More _ il ft' ir -> case someSeparateHead il of
    (h, Nothing) -> more (tupleToSome h) (rotL ft' ir) r
    (h, Just t) -> more (tupleToSome h) (more t ft' ir) r

-- Similar to rotL, but there's no right branch (so rotate to right by 1)
rotR :: Some a -> FingerTree (Tuple a) -> FingerTree a
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
someSeparateHead (Thee x y z) = (x, Just (Two y z))

-- helper for rotL; the first (len-1) elements become the right branch of the
-- inner FT, and the last el becomes the missing right branch of the overall FT
someSeparateLast :: Some a -> (Maybe (Some a), a)
someSeparateLast (One x) = (Nothing, x)
someSeparateLast (Two x y) = (Just (One x), y)
someSeparateLast (Three x y z) = (Just (Two x y), z)

tupleToSome :: Tuple a -> Some a
tupleToSome (Pair _ a b) = Two a b
tupleToSome (Triple _ a b c) = Three a b c

more :: Some a -> FingerTree (Tuple a) -> Some a -> FingerTree a
more l ft r = More (measure l + measure ft + measure r) l ft r

-- splitTree 0 0 More 4 (One 1) Nil (Three 2 3 4)
-- tgtI < 0 + 1, enter first More case
-- splitSome 0 0 l = splitSome 0 0 (One 1) = Split Nil 1 Nil
-- splitTree result = Split Nil 1 (toTree (2 3 4))
--   = Split Nil 1 (More 3 (One 2) Nil (Two 3 4)) ???

-- splitTree :: Int -> Int -> FingerTree a -> Split FingerTree a
-- splitTree _ _ Nil = undefined
-- splitTree _ _ (Unit x) = Split Nil x Nil
-- splitTree tgtI currI (More len l ft r)
--   | tgtI < startMiddle =
--     let Split sl sx sr = splitSome tgtI currI l
--      in Split sl sx (combineTreeLeft sr ft r)
--   | tgtI < startRight = undefined
--   -- let Split ml ft' mr = splitTree tgtI startMiddle ft
--   --     Split il ft'' ir = splitSome tgtI (startMiddle + FingerTree.length ml) ft'
--   --  in Split (combineTreeRight l ml il) ft'' (combineTreeLeft ir mr r)
--   | otherwise =
--     let Split sl sx sr = splitSome tgtI startRight r
--      in Split (combineTreeRight l ft sl) sx sr
--   where
--     startMiddle = currI + measure l
--     startRight = startMiddle + measure ft

-- splitTuple
-- splitTuple :: Int -> Int -> Tuple a -> Split Maybe a
-- splitTuple tgtI currI (Pair l x y)
--   | tgtI < startRight = Split Nothing x (Just (One y)) -- Split Nil x ( y)
--   | otherwise = Split (Just (One x)) y Nothing -- Split (Unit x) y Nil
--   where
--     startRight = currI + measure x
-- splitTuple tgtI currI (Three l x y z)
--   | tgtI < startMiddle = Split Nothing x (Just (Two y z))
--   | tgtI < startRight = Split (Just (One x)) y (Just (One z)) -- Split (Unit x) y (Unit z)
--   | otherwise = Split (Just (Two x y)) z Nothing
--   where
--     startMiddle = currI + measure x
--     startRight = startMiddle + measure y

combineTreeRight :: Some a -> FingerTree (Tuple a) -> FingerTree a -> FingerTree a
combineTreeRight s t Nil =
  let newLen = (1 + measure t + lengthSome s)
   in case (OldFingerTree.last t, removeTail t) of
        (_, Nil) -> someToTree s
        (Just (Pair x y), t') -> More newLen s t' (Two x y)
        (Just (Triple x y z), t') -> More newLen s t' (Three x y z)
        _ -> someToTree s
combineTreeRight s t (Unit x) = More (1 + measure t + lengthSome s) s t (One x)
combineTreeRight s t' (More len l t r) = More newLen s t'' r
  where
    newLen = len + lengthSome s + measure t'
    t'' = insertSome t' l t

combineTreeLeft :: FingerTree a -> FingerTree (Tuple a) -> Some a -> FingerTree a
combineTreeLeft Nil t s =
  let newLen = (1 + measure t + lengthSome s)
   in case (OldFingerTree.head t, OldFingerTree.tail t) of
        (Just (Pair x y), Just v) -> More newLen (Two x y) v s
        (Just (Triple x y z), Just v) -> More newLen (Three x y z) v s
        _ -> someToTree s
combineTreeLeft (Unit x) t' s = More (1 + measure t' + lengthSome s) (One x) t' s
combineTreeLeft (More len l t r) t' s = More newLen l t'' s
  where
    newLen = len + lengthSome s + measure t'
    t'' = insertSome t r t'

someToTree :: Some a -> FingerTree a
someToTree (One x) = Unit x
someToTree (Two x y) = More 2 (One x) Nil (One y)
someToTree (Three x y z) = More 3 (Two x y) Nil (One z)

insertSome :: FingerTree (Tuple a) -> Some a -> FingerTree (Tuple a) -> FingerTree (Tuple a)
insertSome t1 (Two x y) t2 = append (insertTail (Pair x y) t1) t2
insertSome t1 (Three x y z) t2 = append (insertTail (Triple x y z) t1) t2
insertSome t1 (One x) t2 = undefined

lengthSome :: Some a -> Int
lengthSome One {} = 1
lengthSome Two {} = 2
lengthSome Three {} = 3

-- splitTree :: (Measured a v) => (v -> Bool) -> v -> FingerTree v a -> Split (FingerTree v) a
-- -- no way to split a Unit
-- splitTree pred i (Single x) = Split Empty x Empty

-- -- attempt to split a More
-- splitTree pred i (Deep l ft r )
--   | pred startMiddle =
--     let Split sl sx sr = splitSome pred i l in
--       Split (toTree sl) sx (deepL sr ft r )
--   | pred startRight =
--     let Split ml xs mr = splitTree pred startMiddle ft
--         Split sl sx sr = splitSome pred (startMiddle ⊕ kmlk) (toList xs) in
--           Split (deepR l ml sl) sx (deepL sr mr r )
--   | otherwise =
--     let Split sl sx sr = splitSome pred startRight r in
--       Split (deepR l ft sl) sx (toTree sr)
--   where startMiddle = i + (FingerTree.length l)
--         startRight = startMiddle + (FingerTree.length ft)

-- splitSome :: (v -> Bool) -> v -> Some a -> Split FingerTree a
-- splitSome p i (One x) = Split Nil x Nil
-- splitSome p i (Two x y)
--   | p startY = Split Nil x (Unit y)
--   | otherwise = Split (Unit x) y Nil
--   where startY = i + (FingerTree.length x)
-- -- | otherwise = let Split l ft r = splitSome p i' (One y) in
-- --     Split (insertHead x l) ft r
-- splitSome p i (Three x y z)
--   | p i' = Split Nil x (More (FingerTree.length y + FingerTree.length z) (One y) Nil (One z))
--   | otherwise = let Split l ft r = splitSome p i' (Two y z) in
--       Split (insertHead x l) ft r
--   where i' = i + (FingerTree.length x)
-- -- splitSome p i (a : as)
-- --   | p i′ = Split [ ] a as
-- --   | otherwise = let Split l x r = splitSome p i′ as in Split (a : l) x r
-- --   where i′ = i + (length a)

-- split :: Int -> FingerTree a -> (FingerTree a, FingerTree a)
-- split i Nil = (Nil, Nil)
-- split i t = if FingerTree.length t > i then (t, Empty) else (insertTail x l, r)
--   where
--     Split l x r = splitTree i 0 t

instance Monad FingerTree where
  return :: a -> FingerTree a
  return = Unit

  (>>=) :: FingerTree a -> (a -> FingerTree b) -> FingerTree b
  Nil >>= f = Nil
  (Unit _ x) >>= f = f x
  (More n l t r) >>= f = undefined

instance Functor FingerTree where
  fmap :: (a -> b) -> FingerTree a -> FingerTree b
  fmap _ Nil = Nil
  fmap f (Unit x) = Unit (f x)
  fmap f (More n l t r) = More n (fmap l) (fmap (fmap f) t) (fmap r)

instance Functor Some where
  fmap :: (a -> b) -> Some a -> Some b
  fmap f (One x) = One (f x)
  fmap f (Two x y) = Two (f x) (f y)
  fmap f (Three x y z) = Three (f x) (f y) (f z)

instance Functor Tuple where
  fmap :: (a -> b) -> Tuple a -> Tuple b
  fmap f (Pair x y) = Pair (f x) (f y)
  fmap f (Triple x y z) = Triple (f x) (f y) (f z)

--   foldMap _ Empty = mempty
-- foldMap f (Single x) = f x
-- foldMap f (Deep _ pr m sf) =
--   foldMap f pr `mappend` foldMap (foldMap f) m `mappend` foldMap f sf
-- Deep !v !(Digit a) (FingerTree v (Node v a)) !(Digit a)
--   fmap :: (a -> b) -> FingerTree a -> FingerTree b
--   fmap f Nil = Nil
--   fmap f (Unit x) = Unit (f x)
--   fmap f (More i l ft r) =
--     -- TODO FIX
--     More i (mapSome l f) (fmap (toTupleFunction f) ft) (mapSome r f)

mapSome :: Some a -> (a -> b) -> Some b
mapSome (One l x) f = One (f x)
mapSome (Two l x y) f = Two (f x) (f y)
mapSome (Three l x y z) f = Three (f x) (f y) (f z)

-- toTupleFunction :: (a -> b) -> (Tuple a -> Tuple b)
-- toTupleFunction f =
--     (\t ->
--         Pair x y -> Pair (f x) (f y)
--         Triple x y z -> Triple (f x) (f y) (f z)
--     )

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

instance Monoid (FingerTree a) where
  mempty :: FingerTree a
  mempty = Nil

instance Semigroup (FingerTree a) where
  (<>) = append

instance Foldable FingerTree where
  foldMap :: Monoid m => (a -> m) -> FingerTree a -> m
  foldMap f Nil = mempty
  foldMap f (Unit x) = f x
  foldMap f (More n l t r) = foldMap f l `mappend` foldMap (foldMap f) t `mappend` foldMap f r

instance Foldable Tuple where
  foldMap :: Monoid m => (a -> m) -> Tuple a -> m
  foldMap f (Pair x y) = f x `mappend` f y
  foldMap f (Triple x y z) = f x `mappend` f y `mappend` f z

instance Foldable Some where
  foldMap :: Monoid m => (a -> m) -> Tuple a -> m
  foldMap f (One x) = f x
  foldMap f (Two x y) = f x `mappend` f y
  foldMap f (Three x y z) = f x `mappend` f y `mappend` f z

-- foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l

instance Traversable FingerTree where
  traverse :: Applicative z => (a -> z b) -> FingerTree a -> z (FingerTree b)
  traverse f t = undefined

------ Functions ------

-- TODO check i is updated correctly
insertHead :: a -> FingerTree a -> FingerTree a
insertHead a Nil = Unit a
insertHead a (Unit l b) = More (l + measure a) (One a) Nil (One b)
insertHead a (More l (One il b) ft r) =
  More (l + measure a) (Two (il + measure a) a b) ft r
insertHead a (More l (Two il b c) ft r) =
  More (l + measure a) (Three (il + measure a) a b c) ft r
insertHead a (More l (Three il b c d) ft r) =
  More
    (l + measure a)
    (Two (measure a + measure b) a b)
    (insertHead (Pair (measure c + measure d) c d) ft)
    r

-- TODO check i is updated correctly
insertTail :: a -> FingerTree a -> FingerTree a
insertTail z Nil = Unit z
insertTail z (Unit s a) = More 2 (One a) Nil (One z)
insertTail z (More s l ft (One is a)) =
  More (s + measure z) l ft (Two (is + measure z) a z)
insertTail z (More s l ft (Two is a b)) =
  More (s + measure z) l ft (Three (is + measure z) a b z)
insertTail z (More s l ft (Three is a b c)) =
  More
    (s + measure z)
    l
    (insertTail (Pair (measure a + measure b) a b) ft)
    (Two (measure c + measure z) c z)

head :: FingerTree a -> Maybe a
head Nil = Nothing
head (Unit _ x) = Just x
head (More _ (One _ x) _ _) = Just x
head (More _ (Two _ x _) _ _) = Just x
head (More _ (Three _ x _ _) _ _) = Just x

-- TODO check i
tail :: FingerTree a -> Maybe (FingerTree a)
tail (More i (Three _ _ x y) ft r) = Just $ More (i - 1) (Two x y) ft r
tail (More i (Two _ _ x) ft r) = Just $ More (i - 1) (One x) ft r
tail (More i (One _ _) ft r) = case (ft, r) of
  (Nil, One x) -> Just $ Unit x
  (Nil, Two x y) -> Just $ More (i - 1) (One x) Nil (One y)
  (Nil, Three x y z) -> Just $ More (i - 1) (One x) Nil (Two y z)
  _ -> undefined

-- case FingerTree.head ft of
--   Just (Pair x y) -> Just $ More (Two x y) (FingerTree.tail ft) r

last :: FingerTree a -> Maybe a
last Nil = Nothing
last (Unit _ x) = Just x
last (More _ _ _ (One _ x)) = Just x
last (More _ _ _ (Two _ _ x)) = Just x
last (More _ _ _ (Three _ _ _ x)) = Just x

removeTail :: FingerTree a -> FingerTree a
removeTail = undefined

isEmpty :: FingerTree a -> Bool
isEmpty t = undefined

append :: FingerTree a -> FingerTree a -> FingerTree a
append t1 = glue t1 []

glue :: FingerTree a -> [a] -> FingerTree a -> FingerTree a
glue Nil l t2 = foldr insertHead t2 l
glue t1 l Nil = foldl (flip insertTail) t1 l
glue (Unit _ x) l t2 = foldr insertHead t2 (x : l)
glue t1 l (Unit _ y) = foldl (flip insertTail) t1 (l ++ [y])
glue (More i1 x1 t1 y1) l (More _ x2 t2 y2) =
  More i1 x1 (glue t1 (listToTuples (someToList y1 ++ l ++ someToList x2)) t2) y2

someToList :: Some a -> [a]
someToList (One _ x) = [x]
someToList (Two _ x y) = [x, y]

listToTuples :: [a] -> [Tuple a]
listToTuples [] = []
listToTuples [x, y] = [Pair x y]
listToTuples [x, y, z, w] = [Pair x y, Pair z w]
listToTuples (x : y : z : xs) = Triple x y z : listToTuples xs

-- split :: Measured v a => (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
-- split t = undefined

concat :: [FingerTree a] -> FingerTree a
concat l = undefined

-- TODO: Figure out what to do if this contains tuples
toList :: FingerTree a -> [a]
toList Nil = []
toList (Unit _ x) = undefined
toList (More _ l ft r) = undefined

length :: FingerTree a -> Int
length Nil = 0
length (Unit l _) = l
length (More l _ _ _) = l

-- ft8 :: FingerTree Int
-- ft8 = More (One 1) (More (One (Pair 2 3)) Nil (One (Pair 4 5))) (Three 6 7 8)

fromList :: [a] -> FingerTree a
fromList = foldr insertHead Nil

instance (Show a, Arbitrary a) => Arbitrary (FingerTree a) where
  arbitrary = undefined

  shrink :: a -> [a]
  shrink Nil = []
  shrink (Unit _) = [Nil]
  shrink (More n l t r) = someToTreeList l ++ someToTreeList r ++ shrink (fromList (toList t)) ++ [Nil]

someToTreeList :: Some a -> [FingerTree a]
someToTreeList (One _ x) = [Unit x]
someToTreeList (Two _ x y) = [Unit x, Unit y, More 2 (One x) Nil (One y)]

tSplitSome :: Test
tSplitSome =
  TestList
    [ "splitSome singleton" ~: splitSome 0 0 (One 3) ~?= Split Nil 3 Nil,
      "splitSome Two after zero index"
        ~: splitSome 1 0 (Two 3 4) ~?= Split Nil 3 (Unit 4),
      "splitSome Two after nonzero index"
        ~: splitSome 9 8 (Two 3 4) ~?= Split Nil 3 (Unit 4),
      "splitSome Two - no split, target=curent"
        ~: splitSome 0 0 (Two 3 4) ~?= Split (Unit 3) 4 Nil,
      "splitSome Two - no split, diff too big"
        ~: splitSome 3 1 (Two 3 4)
          ~?= Split
            (Unit 3)
            4
            Nil,
      "splitSome Three after first, zero index"
        ~: splitSome 1 0 (Three 4 5 6) ~?= Split Nil 4 (insertHead 5 (Unit 6)),
      "splitSome Three after first, nonzero index"
        ~: splitSome 9 8 (Three 4 5 6) ~?= Split Nil 4 (insertHead 5 (Unit 6)),
      "splitSome Three - split after second"
        ~: splitSome 6 4 (Three 9 10 11) ~?= Split (Unit 9) 10 (Unit 11),
      "splitSome Three - no split, target=current"
        ~: splitSome 4 4 (Three 7 8 9)
        ~?= Split (insertHead 7 (Unit 8)) 9 Nil
    ]

ft1 :: FingerTree Int
ft1 = Unit 1

ft2 :: FingerTree Int
ft2 = More 2 (One 1) Nil (One 2)

ft3 :: FingerTree Int
ft3 = More 3 (One 1) Nil (Two 2 3)

ft4 :: FingerTree Int
ft4 = More 4 (One 1) Nil (Three 2 3 4)

tSplitTree :: Test
tSplitTree =
  TestList
    [ "splitTree len 1" ~: splitTree 0 0 ft1 ~?= Split Nil 1 Nil,
      "splitTree len 1, offset index" ~: splitTree 2 0 ft1 ~?= Split Nil 1 Nil,
      "splitTree len 2 at 0" ~: splitTree 0 0 ft2 ~?= Split (Unit 1) 2 Nil,
      "splitTree len 2 at 0, offset index"
        ~: splitTree 3 3 ft2 ~?= Split (Unit 1) 2 Nil,
      "splitTree len 2 at 1" ~: splitTree 1 0 ft2 ~?= Split Nil 1 (Unit 2),
      "splitTree len 2 at 1, offset index"
        ~: splitTree 3 2 ft2 ~?= Split Nil 1 (Unit 2),
      "splitTree len 3 at 0"
        ~: splitTree 0 0 ft3 ~?= Split (More 2 (One 1) Nil (One 2)) 3 Nil,
      "splitTree len 3 at 1"
        ~: splitTree 1 0 ft3 ~?= Split Nil 1 (More 2 (One 2) Nil (One 3)),
      "splitTree len 3 at 2"
        ~: splitTree 2 0 ft3 ~?= Split (Unit 1) 2 (Unit 3),
      "splitTree len 3 at hi"
        ~: splitTree 3 0 ft3 ~?= Split (More 2 (One 1) Nil (One 2)) 3 Nil,
      "splitTree len 4 at 0"
        ~: splitTree 0 0 ft4 ~?= undefined,
      "splitTree len 4 at 1"
        ~: splitTree 1 0 ft4 ~?= Split Nil 1 (More 3 (One 2) Nil (Two 3 4)),
      "splitTree len 4 at 2"
        ~: splitTree 2 0 ft4 ~?= Split (Unit 1) 2 (More 2 (One 3) Nil (One 4)),
      "splitTree len 4 at 3"
        ~: splitTree 3 0 ft4 ~?= Split (More 2 (One 1) Nil (One 2)) 3 (Unit 4)
    ]
