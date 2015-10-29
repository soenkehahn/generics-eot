{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Generics.Simple (
  datatype,
  Datatype(..),
  Constructor(..),
  Fields(..),

  toEot,
  Eot,
  Void,

  ImpliedByGeneric,

  Generic,
  Rep, -- fixme
  Proxy(..),
  ) where

import           Data.Proxy
import           GHC.Exts (Constraint)
import           GHC.Generics hiding (Datatype, Constructor)

import           Generics.Simple.Datatype
import           Generics.Simple.Eot

datatype :: forall a c f . (Generic a, ImpliedByGeneric a c f) =>
  Proxy a -> Datatype
datatype Proxy = datatypeC (Proxy :: Proxy (Rep a))

toEot :: (Generic a, ImpliedByGeneric a c f) => a -> Eot (Rep a)
toEot = toEotDatatype . from

type family ImpliedByGeneric a c f :: Constraint where
  ImpliedByGeneric a c f =
    (GenericDatatype (Rep a),
     Rep a ~ D1 c f,
     GenericConstructors f,
     EotC (Rep a))
