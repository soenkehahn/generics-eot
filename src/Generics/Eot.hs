{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Eot (
  -- * meta information
  datatype,
  Datatype(..),
  Constructor(..),
  Fields(..),

  -- * converting values
  HasEot(..),
  Void,

  ImpliedByGeneric,

  -- * re-exports
  Generic,
  Proxy(..),
  ) where

import           Data.Proxy
import           GHC.Exts (Constraint)
import           GHC.Generics hiding (Datatype, Constructor)

import           Generics.Eot.Datatype
import           Generics.Eot.Eot

datatype :: forall a c f . (Generic a, ImpliedByGeneric a c f) =>
  Proxy a -> Datatype
datatype Proxy = datatypeC (Proxy :: Proxy (Rep a))

class HasEot a where
  type Eot a :: *
  toEot :: a -> Eot a
  fromEot :: Eot a -> a

instance (Generic a, ImpliedByGeneric a c f) => HasEot a where
  type Eot a = EotG (Rep a)
  toEot = toEotG . from
  fromEot = to . fromEotG

type family ImpliedByGeneric a c f :: Constraint where
  ImpliedByGeneric a c f =
    (GenericDatatype (Rep a),
     Rep a ~ D1 c f,
     GenericConstructors f,
     HasEotG (Rep a))
