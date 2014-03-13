module Control.Trees.LTreeT(TreeT(..)) where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative

import Data.Trees.LTree
data TreeT m a = TreeT (m (Tree a))
runTreeT (TreeT m) = m
treeT0 = TreeT
treeT1 f = treeT0 . f . runTreeT
treeT2 f = treeT1 . f . runTreeT

instance Monad m => Monad (TreeT m) where
    return c =  TreeT $ return $ Leaf c
    tmb_v >>= f = TreeT $ runTreeT tmb_v >>= onone f
               where onone f (Node b_ets) = do
                       ets_ <- mapM (onone f) b_ets
                       return $ Node ets_
                     onone f (Leaf x) = runTreeT $ f x

instance MonadTrans TreeT where
   lift x = TreeT $ x >>= return . Leaf

instance Functor m => Functor (TreeT m) where
   fmap = treeT1 . fmap . fmap

instance Applicative m => Applicative (TreeT m)where
   pure = treeT0 . pure . pure
   (<*>) = treeT2 . liftA2 . liftA2 $ id

instance MonadIO m => MonadIO (TreeT m) where
   liftIO = lift . liftIO
        
