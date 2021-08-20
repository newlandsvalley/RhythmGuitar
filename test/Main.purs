module Test.Main where

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Prelude ((<>), Unit, discard, show)
import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import RhythmGuitar.Normalise (parse)

assertRoundTrip :: String  -> Test
assertRoundTrip s  =
  case (parse s) of
    Right chordSym ->
      Assert.equal s chordSym

    Left err ->
      failure ("parse failed: " <> (show err))

main :: Effect Unit
main = runTest do
  normaliseSuite

normaliseSuite :: Free TestF Unit
normaliseSuite =
  suite "normalise chord symbol" do
    test "A major" do  
      assertRoundTrip "A"
    test "Bb major" do  
      assertRoundTrip "Bb"
    test "C# major" do  
      assertRoundTrip "C#"
    test "D minor" do  
      assertRoundTrip "Dm"
    test "Eb minor" do  
      assertRoundTrip "Ebm"
    test "F# minor" do  
      assertRoundTrip "F#m"
    test "G diminished" do  
      assertRoundTrip "Gdim"
    test "G# diminished" do  
      assertRoundTrip "G#dim"
    test "Ab diminished" do  
      assertRoundTrip "Abdim"
    test "A augmented" do  
      assertRoundTrip "A+"
    test "Bb augmented" do  
      assertRoundTrip "Bb+"
    test "C# augmented" do  
      assertRoundTrip "C#+"
    test "D7" do  
      assertRoundTrip "D7"
    test "Eb7" do  
      assertRoundTrip "Eb7"
    test "F#7" do  
      assertRoundTrip "F#7"
      assertRoundTrip "F#m"
    test "Gm7" do  
      assertRoundTrip "Gm7"
    test "G#m7" do  
      assertRoundTrip "G#m7"
    test "Abm7" do  
      assertRoundTrip "Abm7"