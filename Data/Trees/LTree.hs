-- |
-- Module      : Data.Trees.LTree
-- Copyright   : Rahul Gopinath
-- License     : PublicDomain
-- 
-- Maintainer  : Rahul Gopinath (gopinath@eecs.oregonstate.edu)
-- Stability   : experimental
-- Portability : portable
-- 
-- This Haskell library provides an implementation of a tree data
-- type with content in the leaves.
-- 
module Data.Trees.LTree(Tree(..)) where

import Control.Monad
import Data.Monoid
import Data.Foldable 
import Data.Traversable 
import Data.Typeable

import qualified Data.Traversable as Tr
import qualified Data.Foldable as Fl
import qualified Data.Monoid as Monoid (mappend, mempty, mconcat, Monoid)

import Control.Applicative

-- |
-- | The @LTree c@ is a tree structure with content in the leaves.
data Tree c = Leaf { content :: c }
            | Node { trees :: [Tree c]}
     deriving (Show,Eq)

instance Functor Tree where 
    fmap f (Leaf c) = Leaf $ f c
    fmap f (Node l) = Node $ map (fmap f) l

instance Applicative Tree where
    pure c = Leaf c 
    Node l <*> t = Node (map (<*> t) l)
    Leaf c <*> Node l = Node (map (c <$>) l)
    Leaf c <*> Leaf c' = Leaf (c c')

instance Monad Tree where
    return x = Leaf x
    Node l >>= f = Node (map (>>= f) l)
    Leaf c >>= f = f c

instance Foldable Tree where
    foldMap f (Node l) = foldMap (foldMap f) l
    foldMap f (Leaf c) = f c

instance Traversable Tree where
    traverse f (Node l) = Node <$> traverse (traverse f) l
    traverse f (Leaf c) = Leaf <$> f c


