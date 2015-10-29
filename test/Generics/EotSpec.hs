{-# LANGUAGE DeriveGeneric #-}

module Generics.EotSpec where

import           Test.Hspec

import           Generics.Simple

spec :: Spec
spec = do
  describe "from" $ do
    it "works" $ do
      toEot (A 42 "foo" True ()) `shouldBe`
        Left (42, ("foo", (True, ((), ()))))
      toEot (B True) `shouldBe` Right (Left (True, ()))
      toEot Foo `shouldBe` Left ()

    it "produces the right types" $ do
      let _foo :: Either
            (Int, (String, (Bool, ((), ()))))
            (Either (Bool, ())
             (Either ()
              (Either () Void)))
          _foo = toEot C
      True

data Test
  = A Int String Bool ()
  | B Bool
  | C
  | D
  deriving (Generic)

data Foo = Foo
  deriving (Generic)
