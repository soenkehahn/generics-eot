{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Simple.Eot (
  HasEotG(..),
  Void,
  ) where

import           Data.Proxy
import           GHC.Generics

class HasEotG (a :: * -> *) where
  type EotG a :: *
  toEotG :: a x -> EotG a
  fromEotG :: EotG a -> a x

-- * datatype

instance HasEotG f => HasEotG (D1 c f) where
  type EotG (D1 c f) = EotG f
  toEotG (M1 x) = toEotG x
  fromEotG x = fromEotG x

-- * constructors

instance (HasEotG a, HasEotG b, Normalize (EotG a) (EotG b)) =>
  HasEotG (a :+: b) where
    type EotG (a :+: b) = GEither (EotG a) (EotG b)
    toEotG = \ case
      L1 a -> gLeft (toEotG a) (Proxy :: Proxy (EotG b))
      R1 b -> gRight (Proxy :: Proxy (EotG a)) (toEotG b)

instance EotFieldsC f => HasEotG (C1 c f) where
  type EotG (C1 c f) = Either (EotFields f) Void
  toEotG = Left . toEotFields . unM1

data Void
  deriving (Generic)

deriving instance Show Void
deriving instance Eq Void
deriving instance Ord Void

instance HasEotG V1 where
  type EotG V1 = Void
  toEotG = error "fixme: impossible"

-- * GEither

class Normalize a b where
  type GEither a b :: *
  gLeft :: a -> Proxy b -> GEither a b
  gRight :: Proxy a -> b -> GEither a b

instance Normalize b c => Normalize (Either a b) c where
  type GEither (Either a b) c = Either a (GEither b c)
  gLeft (Left a) Proxy = Left a
  gLeft (Right b) Proxy = Right $ gLeft b (Proxy :: Proxy c)
  gRight Proxy c = Right $ gRight (Proxy :: Proxy b) c

instance Normalize Void b where
  type GEither Void b = b
  gLeft = error "fixme: impossible"
  gRight Proxy b = b

-- * fields

class EotFieldsC (a :: * -> *) where
  type EotFields a :: *
  toEotFields :: a x -> EotFields a

instance (EotFieldsC a, EotFieldsC b, Concat (EotFields a) (EotFields b)) =>
  EotFieldsC (a :*: b) where
    type EotFields (a :*: b) = EotFields a +++ EotFields b
    toEotFields (a :*: b) = toEotFields a +++ toEotFields b

instance EotFieldsC (S1 c (Rec0 f)) where
  type EotFields (S1 c (Rec0 f)) = (f, ())
  toEotFields (M1 (K1 x)) = (x, ())

instance EotFieldsC U1 where
  type EotFields U1 = ()
  toEotFields U1 = ()

-- * heterogenous lists

class Concat a b where
  type a +++ b :: *
  (+++) :: a -> b -> (a +++ b)

instance Concat as bs => Concat (a, as) bs where
  type (a, as) +++ bs = (a, as +++ bs)
  (a, as) +++ bs = (a, as +++ bs)

instance Concat () bs where
  type () +++ bs = bs
  () +++ bs = bs
