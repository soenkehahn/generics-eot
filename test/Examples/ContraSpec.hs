{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Examples.ContraSpec where

import           Test.Hspec

import           Generics.Eot

import           Data.Void
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible

newtype Serializer a = Serializer { runSerializer :: a -> String }

string :: Serializer String
string = Serializer id

int :: Serializer Int
int = Serializer show

instance Contravariant Serializer where
  contramap f (Serializer g) = Serializer (g . f)

instance Divisible Serializer where
  conquer = Serializer (const mempty)

  divide toBC (Serializer sb) (Serializer sc) = Serializer $ \a ->
    case toBC a of
      (b, c) ->
        let bBytes = sb b
            cBytes = sc c
        in bBytes ++ cBytes

instance Decidable Serializer where
  lose f = Serializer $ \a -> absurd (f a)
  choose split l r = Serializer $ \a ->
    either (runSerializer l) (runSerializer r) (split a)

data Identifier = StringId String | IntId Int
  deriving (Eq, Ord, Show, Generic)

identifier :: Serializer Identifier
identifier = contramap toEot $
  chosen (divided string conquer) $
  chosen (divided int conquer) $
  lost

spec :: Spec
spec = do
  describe "contravariant" $ do
    it "allows you to use Decidable with Void" $ do
      runSerializer identifier (StringId "hi") `shouldBe` "hi"
      runSerializer identifier (IntId 4) `shouldBe` "4"
