--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Transf where
  import Control.Monad
  import Data.Bifunctor
  newtype Transf t f u = Transf {transf :: t -> f (t, u)}
  instance Monad f => Applicative (Transf t f) where
    Transf f <*> Transf g = Transf (f >=> \(x, h) -> second h <$> g x)
    pure x = Transf (\y -> return (y, x))
  instance Functor f => Functor (Transf t f) where
    fmap f (Transf g) = Transf (\x -> second f <$> g x)
  instance Monad f => Monad (Transf t f) where
    Transf f >>= g = Transf (f >=> \(x, y) -> transf (g y) x)
--------------------------------------------------------------------------------------------------------------------------------