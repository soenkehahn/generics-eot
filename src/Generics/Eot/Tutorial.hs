{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This tutorial is meant to be read alongside with the haddock comments in
-- 'Generics.Eot'.
--
-- @generics-eot@ allows roughly three different kinds of operations:
--
-- 1. Accessing meta information about ADTs ('datatype' for names, 'Proxy' and
--    'Eot' for field types). Example: Generation of database schemas for ADTs.
-- 2. Deconstructing values generically ('toEot'). Example: Serialization to a
--    binary format.
-- 3. Constructing values of an ADT generically ('fromEot').
--    Example: Deserialization from a binary format.
--
-- Sometimes only one of the three forms is used but often multiple have to
-- be combined. For example serialization to JSON requires both 'datatype' and
-- 'toEot'.

module Generics.Eot.Tutorial where

import           Data.Char
import           Data.List

import           Generics.Eot

-- * 1st Example: Meta Information Without Types: Field Names

-- | This simple function extracts the names of all field selectors and returns
-- them as a list:
--
-- >>> namesOfFields (Proxy :: Proxy A)
-- ["foo","bar","baz"]
namesOfFields :: HasEot a => Proxy a -> [String]
namesOfFields proxy =
  nub $
  concatMap (fieldNames . fields) $
  constructors $ datatype proxy
  where
    fieldNames :: Fields -> [String]
    fieldNames fields = case fields of
      Selectors names -> names
      _ -> []

data A = A1 {
    foo :: String,
    bar :: Int
  }
  | A2 {
    bar :: Int,
    baz :: Bool
  }
  deriving (Generic, Show)

-- * The 'Generic' instance: Don't forget!!!

-- $ To be able to use generic functions that are written with @generics-eot@
-- you need to derive an instance for 'GHC.Generics.Generic' (using
-- @DeriveGeneric@) for your ADTs. This will automatically give you an instance
-- for 'HasEot'.
--
-- When the instance for 'GHC.Generics.Generic' is missing the type error
-- messages are unfortunately very confusing and unhelpful. They go something
-- like this:
--
-- >    Couldn't match type ‘GHC.Generics.Rep WithoutGeneric’
-- >                   with ‘GHC.Generics.D1 c f’
-- >    The type variables ‘c’, ‘f’ are ambiguous
-- >    In the expression: namesOfFields (Proxy :: Proxy WithoutGeneric)
--
-- So don't forget: you need a 'Generic' instance.

-- ** 'Eot': Isomorphic representations

-- $ Part of the type class 'HasEot' is the type-level function 'Eot' that maps
-- ADTs to isomorphic types.
-- These isomorphic types are always a combination of 'Either's, tuples and
-- the uninhabited type 'Void'. For example this type:

data B = B1 Int | B2 String Bool | B3
  deriving (Generic)

-- $ would be mapped to
-- @'Either' ('Int', ()) ('Either' ('String', ('Bool', ())) ('Either' () 'Void'))@.
-- (For the exact rules of this mapping see 'Eot'.)
-- If we have an ADT @a@ then we can convert values of type @a@ to this
-- isomorphic representation
-- @'Eot' a@ with 'toEot' and we can convert in the other direction with
-- 'fromEot'. Generic functions always operate on these isomorphic
-- representations and then convert from or to the real ADTs.
--
-- These generic isomorphic types are referred to as "eot" -- short for
-- "'Either's of tuples" -- in the rest of this tutorial.

-- * 2nd Example: Deconstructing Values: Serialization

-- $ We start by writing a function that operates on the eot representations.
-- The eot representations follow simple patterns and always look similar, but
-- they don't look exactly the same. For this reason we have to use a type
-- class:

class EotSerialize eot where
  eotSerialize :: Int -- ^ The number of the constructor being passed in
    -> eot -- ^ The eot representation
    -> [Int] -- ^ A simple serialization format

-- $ Now we need to write instances for the types that occur in eot types.
-- Usually these are:
--
-- - @'Either' this next@:
--
--     - If as eot value we get @'Left' this@ it means that the original value
--     was constructed with the constructor that corresponds to @this@. In this
--     case we put the number of the constructor into the output and continue
--     with serializing the fields of type @this@.
--     - If we get @'Right' rest@ it means that one of the following
--     constructors was the one that the original value was built with. We
--     continue by increasing the constructor counter and serializing the value
--     of type @rest@.
--
--     Note that this results in 'EotSerialize' class constraints for both
--     @this@ and @rest@. If we write the correct instances for all eot types
--     these constraints should always be fulfilled.

instance (EotSerialize this, EotSerialize rest) =>
  EotSerialize (Either this rest) where

  eotSerialize n (Left fields) = n : eotSerialize n fields
  eotSerialize n (Right rest) = eotSerialize (succ n) rest

-- $
-- - 'Void':
--     We need this instance to make the compiler happy, but it'll never be
--     used. If you look at the type you can also see that: an argument of type
--     'Void' cannot be constructed.

instance EotSerialize Void where
  eotSerialize n void = seq void $ error "impossible"

-- $
-- - @(x, xs)@:
--     Right-nested 2-tuples are used to encode all the fields for one specific
--     constructor. So @x@ is the current field and @xs@ are the remaining
--     fields. To serialize this we serialize x (using 'serialize')
--     and also write the length of the
--     resulting list into the output. This will allow deserialization.
--
--     Note: We could use 'EotSerialize' to serialize the fields. But that would
--     be a bit untrue to the spirit, since the fields are not eot types. Apart
--     from that we might want to encode a field of e.g. type @'Either' a b@
--     differently than the eot type @'Either' a b@. So we use a very similar
--     but distinct type class called 'Serialize'.
--
--     The value of type @xs@ contains the remaining fields and will be encoded
--     recursively with 'EotSerialize'.

instance (Serialize x, EotSerialize xs) => EotSerialize (x, xs) where
  eotSerialize n (x, xs) =
    let xInts = serialize x
    in length xInts : xInts ++ eotSerialize n xs

-- $
-- - @()@:
--     Finally we need an instance for the unit type that marks the end of the
--     fields encoded in 2-tuples. Since @()@ doesn't carry any information, we
--     can encode it as the empty list.

instance EotSerialize () where
  eotSerialize _ () = []

-- | This is the class 'Serialize'. It's used to serialize every field of the
-- used ADTs, so we need instances for all of them.
class Serialize a where
  serialize :: a -> [Int]

instance Serialize Int where
  serialize i = [i]

instance Serialize String where
  serialize = map ord

instance Serialize Bool where
  serialize True = [1]
  serialize False = [0]

instance Serialize () where
  serialize () = []

-- | To tie everything together we provide a function 'genericSerialize' that
-- converts a value of some ADT into an eot value using 'toEot' and then uses
-- 'eotSerialize' to convert that eot value into a list of 'Int's.
genericSerialize :: (HasEot a, EotSerialize (Eot a)) => a -> [Int]
genericSerialize = eotSerialize 0 . toEot

-- $ And it works too:
--
-- >>> genericSerialize (A1 "foo" 42)
-- [0,3,102,111,111,1,42]
-- >>> genericSerialize (A2 23 True)
-- [1,1,23,1,1]

-- * 3rd Example: Constructing Values: Deserialization

-- $ Deserialization works very similarly. It's just that the functions turn
-- lists of 'Int's into eot values.
--
-- Here's the 'EotDeserialize' class with instances for
--
--  - 'Either' this next
--  - 'Void'
--  - (x, xs)
--  - ()

class EotDeserialize eot where
  eotDeserialize :: [Int] -> eot

instance (EotDeserialize this, EotDeserialize next) =>
  EotDeserialize (Either this next) where

  eotDeserialize (0 : r) = Left $ eotDeserialize r
  eotDeserialize (n : r) = Right $ eotDeserialize (pred n : r)

instance EotDeserialize Void where
  eotDeserialize _ = error "invalid input"

instance (Deserialize x, EotDeserialize xs) =>
  EotDeserialize (x, xs) where

  eotDeserialize (len : r) =
    let (this, rest) = splitAt len r
    in (deserialize this, eotDeserialize rest)

instance EotDeserialize () where
  eotDeserialize [] = ()

-- $ And here's the 'Deserialize' plus all instances to deserialize the fields:

class Deserialize a where
  deserialize :: [Int] -> a

instance Deserialize Int where
  deserialize [n] = n

instance Deserialize String where
  deserialize = map chr

instance Deserialize () where
  deserialize [] = ()

instance Deserialize Bool where
  deserialize [0] = False
  deserialize [1] = True

-- | And here's 'genericDeserialize' to tie it together. It uses
-- 'eotDeserialize' to convert a list of 'Int's into an eot value and then
-- 'fromEot' to construct a value of the wanted ADT.
genericDeserialize :: (HasEot a, EotDeserialize (Eot a)) => [Int] -> a
genericDeserialize = fromEot . eotDeserialize

-- $ Here you can see it in action:
--
-- >>> genericDeserialize [0,3,102,111,111,1,42] :: A
-- A1 {foo = "foo", bar = 42}
-- >>> genericDeserialize [1,1,23,1,1] :: A
-- A2 {bar = 23, baz = True}
--
-- And it is the inverse of 'genericSerialize':
--
-- >>> (genericDeserialize $ genericSerialize $ A1 "foo" 42) :: A
-- A1 {foo = "foo", bar = 42}

-- * DefaultSignatures

-- * Meta Data with types
