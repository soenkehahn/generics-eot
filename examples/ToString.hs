{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ToString (ToString(..)) where

import           Data.List
import           Generics.Simple

class ToString a where
  toString :: a -> String
  default toString :: (Generic a, ImpliedByGeneric a c f, ToStringG (Eot (Rep a))) =>
    a -> String
  toString = toStringG

instance ToString Bool where
  toString = show

instance ToString Int where
  toString = show

instance ToString () where
  toString = show

toStringG :: forall a c f . (Generic a, ImpliedByGeneric a c f, ToStringG (Eot (Rep a))) =>
  a -> String
toStringG a =
  toStringConss
    (constructors (datatype (Proxy :: Proxy a)))
    (toEot a)

class ToStringG a where
  toStringConss :: [Constructor] -> a -> String

instance (ToStringFields a, ToStringG b) => ToStringG (Either a b) where
  toStringConss (Constructor name fieldMeta : _) (Left fields) =
    name ++ format (toStringFields fields)
    where
      format fieldStrings = case fieldMeta of
        Selectors names ->
          " {" ++ intercalate ", "
            (zipWith (\ sel v -> sel ++ " = " ++ v) names fieldStrings) ++
          "}"
        NoSelectors _ -> " " ++ unwords fieldStrings
        NoFields -> ""
  toStringConss (_ : r) (Right next) =
    toStringConss r next
  toStringConss [] _ = error "fixme: impossible"

instance ToStringG Void where
  toStringConss = error "fixme: impossible"

class ToStringFields a where
  toStringFields :: a -> [String]

instance (ToString x, ToStringFields xs) => ToStringFields (x, xs) where
  toStringFields (x, xs) = toString x : toStringFields xs

instance ToStringFields () where
  toStringFields () = []
