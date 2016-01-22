
module Generics.Eot.TutorialSpec where

import           Test.DocTest
import           Test.Hspec

import           Generics.Eot.Tutorial ()

spec :: Spec
spec = describe "tutorial" $ do
  it "doctests" $ do
    pending
    doctest (words "src/Generics/Eot/Tutorial.lhs -isrc")
