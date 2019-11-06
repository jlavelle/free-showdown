{-# LANGUAGE ExistentialQuantification #-}

module FreeCoyo where

import Prelude hiding (filter)

import Control.Monad.Trans.Free
import Data.Functor.Of
import Control.Monad (void)

data Coyoneda f a = forall x. Coyoneda (f x) (x -> a)

instance Functor (Coyoneda f) where
  fmap f (Coyoneda fx xa) = Coyoneda fx (f . xa)

type Stream a m b = FreeT (Coyoneda (Of a)) m b

filter :: Monad m => (a -> Bool) -> Stream a m b -> Stream a m b
filter p (FreeT x) = FreeT $ x >>= \case
  Pure b -> pure $ Pure b
  Free (Coyoneda (a :> r) rest)
    | p a -> pure $ Free (Coyoneda (a :> r) (filter p . rest))
    | otherwise -> runFreeT $ filter p (rest r)

unfoldr :: Monad m => (s -> m (Either r (a, s))) -> s -> Stream a m r
unfoldr step = loop
  where
    loop s0 = FreeT $ do
      e <- step s0
      case e of
        Left r -> pure (Pure r)
        Right (a, s) -> pure (Free (Coyoneda (a :> s) loop))

ints :: Monad m => Int -> Stream Int m ()
ints m = unfoldr go 0
  where
    go n | n >= m = pure $ Left ()
         | otherwise = pure $ Right (n, n + 1)

runStream :: Monad m => Stream a m r -> m ()
runStream = void . iterT (\(Coyoneda (_ :> r) f) -> f r)

filterBench :: Monad m => Int -> m ()
filterBench n = runStream $ filter even $ ints n
