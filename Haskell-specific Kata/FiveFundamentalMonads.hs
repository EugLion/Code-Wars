{-# LANGUAGE NoImplicitPrelude #-}

module FiveFundamentalMonadsKata where

-- | Implement the five basic Monads (Id, Maybe, Reader, State, Writer) (4 kyu)
-- | Link: https://biturl.io/5Monads

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

-- | My original solution
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

newtype Identity a = Identity a

data Maybe a = Nothing | Just a deriving (Show, Eq)

newtype State s a = State {runState :: s -> (a, s)}

newtype Reader s a = Reader {runReader :: s -> a}

newtype Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return = State . (\ x s -> (x, s))
  (State g) >>= f = State $ \s ->
    let (v, s') = g s in (runState $ f v) s'

instance Monad (Reader s) where
  return = Reader . const
  (Reader g) >>= f = Reader $ \s ->
    let r = f (g s) in runReader r s

instance Monoid w => Monad (Writer w) where
  return x = Writer (mempty, x)
  (Writer (s, v)) >>= f = Writer $
    let (s', v') = runWriter $ f v in (s <> s', v')

