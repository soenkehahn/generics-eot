{-# LANGUAGE CPP #-}

module Generics.Eot.TutorialSpec where

#if MIN_VERSION_base(4,9,0)

import           Test.DocTest
import           Test.Hspec

import           Generics.Eot.Tutorial ()

spec :: Spec
spec = describe "tutorial" $ do

  it "doctests" $ do
    doctest (words "src/Generics/Eot/Tutorial.lhs -isrc -pgmL markdown-unlit")

#else

import           Test.Hspec

spec :: Spec
spec = return ()

#endif
