{-# LANGUAGE FlexibleContexts #-}

module MinBound (minBoundG) where

import           Generics.Eot

minBoundG :: (Generic a, ImpliedByGeneric a c f, GMinBound (Eot a)) => a
minBoundG = fromEot gMinBound

class GMinBound a where
  gMinBound :: a

instance GMinBound a => GMinBound (Either a x) where
  gMinBound = Left gMinBound

instance (Bounded f, GMinBound fs) =>
  GMinBound (f, fs) where
    gMinBound = (minBound, gMinBound)

instance GMinBound () where
  gMinBound = ()
