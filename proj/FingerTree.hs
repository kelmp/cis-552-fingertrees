{-# LANGUAGE InstanceSigs #-}

module FingerTree() where

import Control.Monad
import Control.Applicative

instance Functor FingerTree where
    fmap :: (a -> b) -> FingerTree a -> FingerTree b
    fmap f t = undefined

instance Applicative FingerTree where
    pure t = undefined
    t1 <*> t2 = undefined
-- Needs to implement:
-- Functor
-- Applicative
-- Monad
-- Monoid
-- Foldable
-- Traversable


-- data Seq a = Nil
-- | Unit a
-- | More (Some a) (Seq (Tuple a)) (Some a)

-- data Some a = One a
-- | Two a a
-- | Three a a a

-- data Tuple a = Pair a a
-- | Triple a a a
