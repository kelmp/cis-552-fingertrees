{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HackageFT where

-- #if MIN_VERSION_base(4,6,0)
-- import GHC.Generics
-- #endif
-- #if MIN_VERSION_base(4,8,0)
-- import qualified Prelude (null)
-- #else
-- import Control.Applicative (Applicative (pure, (<*>)), (<$>))
-- -- #endif
-- -- #if (MIN_VERSION_base(4,9,0)) && !(MIN_VERSION_base(4,11,0))

-- -- #endif
-- import Data.Foldable (Foldable (foldMap), toList)
-- import Data.Monoid
-- import Data.Semigroup
-- import Prelude hiding (null, reverse)
-- import GHC.Generics

data FingerTree v a
  = Empty

--   | Single a
--   | Deep !v !(Digit a) (FingerTree v (Node v a)) !(Digit a)

-- data Node v a = Node2 !v a a | Node3 !v a a a
--   deriving (Show, Eq)

-- data Digit a
--   = One a
--   | Two a a
--   | Three a a a
--   | Four a a a a
--   deriving (Show, Eq)

-- data ViewR s a
--     = EmptyR        -- ^ empty sequence
--     | s a :> a      -- ^ the sequence minus the rightmost element,
--                     -- and the rightmost element
--     deriving (Eq, Ord, Show, Read)

-- -- | View of the left end of a sequence.
-- data ViewL s a
--     = EmptyL        -- ^ empty sequence
--     | a :< s a      -- ^ leftmost element and the rest of the sequence
--     deriving (Eq, Ord, Show, Read)

-- deep ::
--   (Measured v a) =>
--   Digit a ->
--   FingerTree v (Node v a) ->
--   Digit a ->
--   FingerTree v a
-- deep pr m sf =
--   Deep ((measure pr `mappend` measure m) `mappend` measure sf) pr m sf

-- -- | Elements from left to right.
-- instance Foldable (FingerTree v) where
--   foldMap _ Empty = mempty
--   foldMap f (Single x) = f x
--   foldMap f (Deep _ pr m sf) =
--     foldMap f pr `mappend` foldMap (foldMap f) m `mappend` foldMap f sf

-- instance (Eq a) => Eq (FingerTree v a) where
--   xs == ys = toList xs == toList ys

-- -- | Lexicographical order from left to right.
-- instance (Ord a) => Ord (FingerTree v a) where
--   compare xs ys = compare (toList xs) (toList ys)

-- instance (Show a) => Show (FingerTree v a) where
--     showsPrec p xs = showParen (p > 10) $
--         showString "fromList " . shows (toList xs)

-- instance Foldable Digit where
--   foldMap f (One a) = f a
--   foldMap f (Two a b) = f a `mappend` f b
--   foldMap f (Three a b c) = f a `mappend` f b `mappend` f c
--   foldMap f (Four a b c d) = f a `mappend` f b `mappend` f c `mappend` f d

-- class (Monoid v) => Measured v a | a -> v where
--   measure :: a -> v

-- instance (Measured v a) => Measured v (Digit a) where
--   measure = foldMap measure

-- -- | /O(1)/. The cached measure of a tree.
-- instance (Measured v a) => Measured v (FingerTree v a) where
--   measure Empty = mempty
--   measure (Single x) = measure x
--   measure (Deep v _ _ _) = v

-- instance Foldable (Node v) where
--     foldMap f (Node2 _ a b) = f a `mappend` f b
--     foldMap f (Node3 _ a b c) = f a `mappend` f b `mappend` f c

-- node2        ::  (Measured v a) => a -> a -> Node v a
-- node2 a b    =   Node2 (measure a `mappend` measure b) a b

-- node3        ::  (Measured v a) => a -> a -> a -> Node v a
-- node3 a b c  =   Node3 (measure a `mappend` measure b `mappend` measure c) a b c

-- instance (Monoid v) => Measured v (Node v a) where
--     measure (Node2 v _ _)    =  v
--     measure (Node3 v _ _ _)  =  v

-- nodeToDigit :: Node v a -> Digit a
-- nodeToDigit (Node2 _ a b) = Two a b
-- nodeToDigit (Node3 _ a b c) = Three a b c

-- -- | /O(log(min(i,n-i)))/. Split a sequence at a point where the predicate
-- -- on the accumulated measure of the prefix changes from 'False' to 'True'.
-- --
-- -- For predictable results, one should ensure that there is only one such
-- -- point, i.e. that the predicate is /monotonic/.
-- split ::
--   (Measured v a) =>
--   (v -> Bool) ->
--   FingerTree v a ->
--   (FingerTree v a, FingerTree v a)
-- split _ Empty = (Empty, Empty)
-- split p xs
--   | p (measure xs) = (l, x <| r)
--   | otherwise = (xs, Empty)
--   where
--     Split l x r = splitTree p mempty xs

-- -- | /O(log(min(i,n-i)))/.
-- -- Given a monotonic predicate @p@, @'takeUntil' p t@ is the largest
-- -- prefix of @t@ whose measure does not satisfy @p@.
-- --
-- -- *  @'takeUntil' p t = 'fst' ('split' p t)@
-- takeUntil :: (Measured v a) => (v -> Bool) -> FingerTree v a -> FingerTree v a
-- takeUntil p = fst . split p

-- -- | /O(log(min(i,n-i)))/.
-- -- Given a monotonic predicate @p@, @'dropUntil' p t@ is the rest of @t@
-- -- after removing the largest prefix whose measure does not satisfy @p@.
-- --
-- -- * @'dropUntil' p t = 'snd' ('split' p t)@
-- dropUntil :: (Measured v a) => (v -> Bool) -> FingerTree v a -> FingerTree v a
-- dropUntil p = snd . split p

-- data Split t a = Split t a t

-- splitTree ::
--   (Measured v a) =>
--   (v -> Bool) ->
--   v ->
--   FingerTree v a ->
--   Split (FingerTree v a) a
-- splitTree _ _ Empty = undefined -- NOT POSSIBLE
-- splitTree _ _ (Single x) = Split Empty x Empty
-- splitTree p i (Deep _ pr m sf)
--   | p vpr =
--     let Split l x r = splitDigit p i pr
--      in Split (maybe Empty digitToTree l) x (deepL r m sf)
--   | p vm =
--     let Split ml xs mr = splitTree p vpr m
--         Split l x r = splitNode p (vpr `mappend` measure ml) xs
--      in Split (deepR pr ml l) x (deepL r mr sf)
--   | otherwise =
--     let Split l x r = splitDigit p vm sf
--      in Split (deepR pr m l) x (maybe Empty digitToTree r)
--   where
--     vpr = i `mappend` measure pr
--     vm = vpr `mappend` measure m

-- deepL ::
--   (Measured v a) =>
--   Maybe (Digit a) ->
--   FingerTree v (Node v a) ->
--   Digit a ->
--   FingerTree v a
-- deepL Nothing m sf = rotL m sf
-- deepL (Just pr) m sf = deep pr m sf

-- deepR ::
--   (Measured v a) =>
--   Digit a ->
--   FingerTree v (Node v a) ->
--   Maybe (Digit a) ->
--   FingerTree v a
-- deepR pr m Nothing = rotR pr m
-- deepR pr m (Just sf) = deep pr m sf

-- splitNode ::
--   (Measured v a) =>
--   (v -> Bool) ->
--   v ->
--   Node v a ->
--   Split (Maybe (Digit a)) a
-- splitNode p i (Node2 _ a b)
--   | p va = Split Nothing a (Just (One b))
--   | otherwise = Split (Just (One a)) b Nothing
--   where
--     va = i `mappend` measure a
-- splitNode p i (Node3 _ a b c)
--   | p va = Split Nothing a (Just (Two b c))
--   | p vab = Split (Just (One a)) b (Just (One c))
--   | otherwise = Split (Just (Two a b)) c Nothing
--   where
--     va = i `mappend` measure a
--     vab = va `mappend` measure b

-- splitDigit ::
--   (Measured v a) =>
--   (v -> Bool) ->
--   v ->
--   Digit a ->
--   Split (Maybe (Digit a)) a
-- splitDigit _ i (One a) = i `seq` Split Nothing a Nothing
-- splitDigit p i (Two a b)
--   | p va = Split Nothing a (Just (One b))
--   | otherwise = Split (Just (One a)) b Nothing
--   where
--     va = i `mappend` measure a
-- splitDigit p i (Three a b c)
--   | p va = Split Nothing a (Just (Two b c))
--   | p vab = Split (Just (One a)) b (Just (One c))
--   | otherwise = Split (Just (Two a b)) c Nothing
--   where
--     va = i `mappend` measure a
--     vab = va `mappend` measure b
-- splitDigit p i (Four a b c d)
--   | p va = Split Nothing a (Just (Three b c d))
--   | p vab = Split (Just (One a)) b (Just (Two c d))
--   | p vabc = Split (Just (Two a b)) c (Just (One d))
--   | otherwise = Split (Just (Three a b c)) d Nothing
--   where
--     va = i `mappend` measure a
--     vab = va `mappend` measure b
--     vabc = vab `mappend` measure c

--  -- functions:
--  -- | /O(1)/. Add an element to the left end of a sequence.
-- -- Mnemonic: a triangle with the single element at the pointy end.
-- (<|) :: (Measured v a) => a -> FingerTree v a -> FingerTree v a
-- a <| Empty              =  Single a
-- a <| Single b           =  deep (One a) Empty (One b)
-- a <| Deep v (Four b c d e) m sf = m `seq`
--     Deep (measure a `mappend` v) (Two a b) (node3 c d e <| m) sf
-- a <| Deep v pr m sf     =
--     Deep (measure a `mappend` v) (consDigit a pr) m sf

-- consDigit :: a -> Digit a -> Digit a
-- consDigit a (One b) = Two a b
-- consDigit a (Two b c) = Three a b c
-- consDigit a (Three b c d) = Four a b c d
-- consDigit _ (Four _ _ _ _) = undefined -- TODO

-- -- | /O(1)/. Add an element to the right end of a sequence.
-- -- Mnemonic: a triangle with the single element at the pointy end.
-- (|>) :: (Measured v a) => FingerTree v a -> a -> FingerTree v a
-- Empty |> a              =  Single a
-- Single a |> b           =  deep (One a) Empty (One b)
-- Deep v pr m (Four a b c d) |> e = m `seq`
--     Deep (v `mappend` measure e) pr (m |> node3 a b c) (Two d e)
-- Deep v pr m sf |> x     =
--     Deep (v `mappend` measure x) pr m (snocDigit sf x)

-- snocDigit :: Digit a -> a -> Digit a
-- snocDigit (One a) b = Two a b
-- snocDigit (Two a b) c = Three a b c
-- snocDigit (Three a b c) d = Four a b c d
-- snocDigit (Four _ _ _ _) _ = undefined -- TODO

-- -- | /O(1)/. Is this the empty sequence?
-- null :: FingerTree v a -> Bool
-- null Empty = True
-- null _ = False

-- -- | /O(log(min(n1,n2)))/. Concatenate two sequences.
-- (><) :: (Measured v a) => FingerTree v a -> FingerTree v a -> FingerTree v a
-- (><) =  appendTree0

-- appendTree0 :: (Measured v a) => FingerTree v a -> FingerTree v a -> FingerTree v a
-- appendTree0 Empty xs =
--     xs
-- appendTree0 xs Empty =
--     xs
-- appendTree0 (Single x) xs =
--     x <| xs
-- appendTree0 xs (Single x) =
--     xs |> x
-- appendTree0 (Deep _ pr1 m1 sf1) (Deep _ pr2 m2 sf2) =
--     deep pr1 (addDigits0 m1 sf1 pr2 m2) sf2

-- addDigits0 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
-- addDigits0 m1 (One a) (One b) m2 =
--     appendTree1 m1 (node2 a b) m2
-- addDigits0 m1 (One a) (Two b c) m2 =
--     appendTree1 m1 (node3 a b c) m2
-- addDigits0 m1 (One a) (Three b c d) m2 =
--     appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits0 m1 (One a) (Four b c d e) m2 =
--     appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits0 m1 (Two a b) (One c) m2 =
--     appendTree1 m1 (node3 a b c) m2
-- addDigits0 m1 (Two a b) (Two c d) m2 =
--     appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits0 m1 (Two a b) (Three c d e) m2 =
--     appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits0 m1 (Two a b) (Four c d e f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits0 m1 (Three a b c) (One d) m2 =
--     appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits0 m1 (Three a b c) (Two d e) m2 =
--     appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits0 m1 (Three a b c) (Three d e f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits0 m1 (Three a b c) (Four d e f g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits0 m1 (Four a b c d) (One e) m2 =
--     appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits0 m1 (Four a b c d) (Two e f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits0 m1 (Four a b c d) (Three e f g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits0 m1 (Four a b c d) (Four e f g h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2

-- appendTree1 :: (Measured v a) => FingerTree v a -> a -> FingerTree v a -> FingerTree v a
-- appendTree1 Empty a xs =
--     a <| xs
-- appendTree1 xs a Empty =
--     xs |> a
-- appendTree1 (Single x) a xs =
--     x <| a <| xs
-- appendTree1 xs a (Single x) =
--     xs |> a |> x
-- appendTree1 (Deep _ pr1 m1 sf1) a (Deep _ pr2 m2 sf2) =
--     deep pr1 (addDigits1 m1 sf1 a pr2 m2) sf2

-- addDigits1 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
-- addDigits1 m1 (One a) b (One c) m2 =
--     appendTree1 m1 (node3 a b c) m2
-- addDigits1 m1 (One a) b (Two c d) m2 =
--     appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits1 m1 (One a) b (Three c d e) m2 =
--     appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits1 m1 (One a) b (Four c d e f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits1 m1 (Two a b) c (One d) m2 =
--     appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits1 m1 (Two a b) c (Two d e) m2 =
--     appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits1 m1 (Two a b) c (Three d e f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits1 m1 (Two a b) c (Four d e f g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits1 m1 (Three a b c) d (One e) m2 =
--     appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits1 m1 (Three a b c) d (Two e f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits1 m1 (Three a b c) d (Three e f g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits1 m1 (Three a b c) d (Four e f g h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits1 m1 (Four a b c d) e (One f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits1 m1 (Four a b c d) e (Two f g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits1 m1 (Four a b c d) e (Three f g h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits1 m1 (Four a b c d) e (Four f g h i) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2

-- appendTree2 :: (Measured v a) => FingerTree v a -> a -> a -> FingerTree v a -> FingerTree v a
-- appendTree2 Empty a b xs =
--     a <| b <| xs
-- appendTree2 xs a b Empty =
--     xs |> a |> b
-- appendTree2 (Single x) a b xs =
--     x <| a <| b <| xs
-- appendTree2 xs a b (Single x) =
--     xs |> a |> b |> x
-- appendTree2 (Deep _ pr1 m1 sf1) a b (Deep _ pr2 m2 sf2) =
--     deep pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2

-- addDigits2 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
-- addDigits2 m1 (One a) b c (One d) m2 =
--     appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits2 m1 (One a) b c (Two d e) m2 =
--     appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits2 m1 (One a) b c (Three d e f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits2 m1 (One a) b c (Four d e f g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits2 m1 (Two a b) c d (One e) m2 =
--     appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits2 m1 (Two a b) c d (Two e f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits2 m1 (Two a b) c d (Three e f g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits2 m1 (Two a b) c d (Four e f g h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits2 m1 (Three a b c) d e (One f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits2 m1 (Three a b c) d e (Two f g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits2 m1 (Three a b c) d e (Three f g h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits2 m1 (Three a b c) d e (Four f g h i) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits2 m1 (Four a b c d) e f (One g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits2 m1 (Four a b c d) e f (Two g h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits2 m1 (Four a b c d) e f (Three g h i) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 =
--     appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2

-- appendTree3 :: (Measured v a) => FingerTree v a -> a -> a -> a -> FingerTree v a -> FingerTree v a
-- appendTree3 Empty a b c xs =
--     a <| b <| c <| xs
-- appendTree3 xs a b c Empty =
--     xs |> a |> b |> c
-- appendTree3 (Single x) a b c xs =
--     x <| a <| b <| c <| xs
-- appendTree3 xs a b c (Single x) =
--     xs |> a |> b |> c |> x
-- appendTree3 (Deep _ pr1 m1 sf1) a b c (Deep _ pr2 m2 sf2) =
--     deep pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2

-- addDigits3 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
-- addDigits3 m1 (One a) b c d (One e) m2 =
--     appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits3 m1 (One a) b c d (Two e f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits3 m1 (One a) b c d (Three e f g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits3 m1 (One a) b c d (Four e f g h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits3 m1 (Two a b) c d e (One f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits3 m1 (Two a b) c d e (Two f g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits3 m1 (Two a b) c d e (Three f g h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits3 m1 (Two a b) c d e (Four f g h i) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits3 m1 (Three a b c) d e f (One g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits3 m1 (Three a b c) d e f (Two g h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits3 m1 (Three a b c) d e f (Three g h i) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits3 m1 (Three a b c) d e f (Four g h i j) m2 =
--     appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
-- addDigits3 m1 (Four a b c d) e f g (One h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits3 m1 (Four a b c d) e f g (Two h i) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits3 m1 (Four a b c d) e f g (Three h i j) m2 =
--     appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
-- addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 =
--     appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

-- appendTree4 :: (Measured v a) => FingerTree v a -> a -> a -> a -> a -> FingerTree v a -> FingerTree v a
-- appendTree4 Empty a b c d xs =
--     a <| b <| c <| d <| xs
-- appendTree4 xs a b c d Empty =
--     xs |> a |> b |> c |> d
-- appendTree4 (Single x) a b c d xs =
--     x <| a <| b <| c <| d <| xs
-- appendTree4 xs a b c d (Single x) =
--     xs |> a |> b |> c |> d |> x
-- appendTree4 (Deep _ pr1 m1 sf1) a b c d (Deep _ pr2 m2 sf2) =
--     deep pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2

-- addDigits4 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
-- addDigits4 m1 (One a) b c d e (One f) m2 =
--     appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits4 m1 (One a) b c d e (Two f g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits4 m1 (One a) b c d e (Three f g h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits4 m1 (One a) b c d e (Four f g h i) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits4 m1 (Two a b) c d e f (One g) m2 =
--     appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits4 m1 (Two a b) c d e f (Two g h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits4 m1 (Two a b) c d e f (Three g h i) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits4 m1 (Two a b) c d e f (Four g h i j) m2 =
--     appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
-- addDigits4 m1 (Three a b c) d e f g (One h) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits4 m1 (Three a b c) d e f g (Two h i) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits4 m1 (Three a b c) d e f g (Three h i j) m2 =
--     appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
-- addDigits4 m1 (Three a b c) d e f g (Four h i j k) m2 =
--     appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
-- addDigits4 m1 (Four a b c d) e f g h (One i) m2 =
--     appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits4 m1 (Four a b c d) e f g h (Two i j) m2 =
--     appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
-- addDigits4 m1 (Four a b c d) e f g h (Three i j k) m2 =
--     appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
-- addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 =
--     appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2

-- -- Helper functions i dont understand fully:
-- -- | /O(1)/. Analyse the right end of a sequence.
-- viewr :: (Measured v a) => FingerTree v a -> ViewR (FingerTree v) a
-- viewr Empty                     =  EmptyR
-- viewr (Single x)                =  Empty :> x
-- viewr (Deep _ pr m (One x))     =  rotR pr m :> x
-- viewr (Deep _ pr m sf)          =  deep pr m (rtailDigit sf) :> rheadDigit sf

-- rotR :: (Measured v a) => Digit a -> FingerTree v (Node v a) -> FingerTree v a
-- rotR pr m = case viewr m of
--     EmptyR  ->  digitToTree pr
--     m' :> a ->  Deep (measure pr `mappend` measure m) pr m' (nodeToDigit a)

-- rheadDigit :: Digit a -> a
-- rheadDigit (One a) = a
-- rheadDigit (Two _ b) = b
-- rheadDigit (Three _ _ c) = c
-- rheadDigit (Four _ _ _ d) = d

-- rtailDigit :: Digit a -> Digit a
-- rtailDigit (One _) = undefined -- TODO
-- rtailDigit (Two a _) = One a
-- rtailDigit (Three a b _) = Two a b
-- rtailDigit (Four a b c _) = Three a b c

-- digitToTree :: (Measured v a) => Digit a -> FingerTree v a
-- digitToTree (One a) = Single a
-- digitToTree (Two a b) = deep (One a) Empty (One b)
-- digitToTree (Three a b c) = deep (Two a b) Empty (One c)
-- digitToTree (Four a b c d) = deep (Two a b) Empty (Two c d)

-- -- | /O(1)/. Analyse the left end of a sequence.
-- viewl :: (Measured v a) => FingerTree v a -> ViewL (FingerTree v) a
-- viewl Empty                     =  EmptyL
-- viewl (Single x)                =  x :< Empty
-- viewl (Deep _ (One x) m sf)     =  x :< rotL m sf
-- viewl (Deep _ pr m sf)          =  lheadDigit pr :< deep (ltailDigit pr) m sf

-- rotL :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> FingerTree v a
-- rotL m sf      =   case viewl m of
--     EmptyL  ->  digitToTree sf
--     a :< m' ->  Deep (measure m `mappend` measure sf) (nodeToDigit a) m' sf

-- lheadDigit :: Digit a -> a
-- lheadDigit (One a) = a
-- lheadDigit (Two a _) = a
-- lheadDigit (Three a _ _) = a
-- lheadDigit (Four a _ _ _) = a

-- ltailDigit :: Digit a -> Digit a
-- ltailDigit (One _) = undefined -- TODO
-- ltailDigit (Two _ b) = One b
-- ltailDigit (Three _ b c) = Two b c
-- ltailDigit (Four _ b c d) = Three b c d