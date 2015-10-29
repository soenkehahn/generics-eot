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

module Generics.Simple.Eot where

import           Data.Proxy
import           GHC.Generics

-- * datatype

class EotC (a :: * -> *) where
  type Eot a :: *
  toEotDatatype :: a x -> Eot a

instance (EotConssC f) => EotC (D1 c f) where
  type Eot (D1 c f) = EotConss f
  -- fixme: remove Eot?
  toEotDatatype (M1 x) = toEotConss x

-- * constructors

class EotConssC (a :: * -> *) where
  type EotConss a :: *
  toEotConss :: a x -> EotConss a

instance (EotConssC a, EotConssC b, Normalize (EotConss a) (EotConss b)) =>
  EotConssC (a :+: b) where
    type EotConss (a :+: b) = GEither (EotConss a) (EotConss b)
    toEotConss = \ case
      L1 a -> gLeft (toEotConss a) (Proxy :: Proxy (EotConss b))
      R1 b -> gRight (Proxy :: Proxy (EotConss a)) (toEotConss b)

instance EotFieldsC f => EotConssC (C1 c f) where
  type EotConss (C1 c f) = Either (EotFields f) Void
  toEotConss = Left . toEotFields . unM1

data Void
  deriving (Generic)

deriving instance Show Void
deriving instance Eq Void
deriving instance Ord Void

instance EotConssC V1 where
  type EotConss V1 = Void
  toEotConss = error "fixme: impossible"

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
