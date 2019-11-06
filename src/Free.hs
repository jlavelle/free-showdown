module Free where

import Prelude hiding (filter)

import Control.Monad.Trans.Free
import Data.Functor.Of
import Control.Monad (void)

type Stream a m b = FreeT (Of a) m b

filter :: Monad m => (a -> Bool) -> Stream a m b -> Stream a m b
filter p (FreeT x) = FreeT $ x >>= \case
  Pure b -> pure $ Pure b
  Free (a :> r)
    | p a -> pure $ Free (a :> filter p r)
    | otherwise -> runFreeT $ filter p r

unfoldr :: Monad m => (s -> m (Either r (a, s))) -> s -> Stream a m r
unfoldr step = loop
  where
    loop s0 = FreeT $ do
      e <- step s0
      case e of
        Left r -> pure (Pure r)
        Right (a, s) -> pure (Free (a :> loop s))

ints :: Monad m => Int -> Stream Int m ()
ints m = unfoldr go 0
  where
    go n | n >= m = pure $ Left ()
         | otherwise = pure $ Right (n, n + 1)

runStream :: Monad m => Stream a m r -> m ()
runStream = void . iterT (\(_ :> r) -> r)

filterBench :: Monad m => Int -> m ()
filterBench n = runStream $ filter even $ ints n
