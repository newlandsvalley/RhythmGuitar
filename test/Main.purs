module Test.Main where

import Control.Monad.Free (Free)
import Data.Either (Either(..))
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
      
assertCanonical :: String -> String -> Test
assertCanonical canonical s  =
  case (parse s) of
    Right chordSym ->
      Assert.equal canonical chordSym

    Left err ->
      failure ("parse failed: " <> (show err))

main :: Effect Unit
main = runTest do
  identitySuite
  normaliseSuite
  enharmonicEquivalenceSuite
  ignoredSlashNoteSuite

identitySuite :: Free TestF Unit
identitySuite =
  suite "chord symbols requiring no normalisation" do
    test "A major" do  
      assertRoundTrip "A"
    test "C# major" do  
      assertRoundTrip "C#"
    test "D minor" do  
      assertRoundTrip "Dm"
    test "F# minor" do  
      assertRoundTrip "F#m"
    test "G diminished" do  
      assertRoundTrip "Gdim"
    test "G# diminished" do  
      assertRoundTrip "G#dim"
    test "A augmented" do  
      assertRoundTrip "A+"
    test "C suspended" do  
      assertRoundTrip "Csus"
    test "C# augmented" do  
      assertRoundTrip "C#+"
    test "D7" do  
      assertRoundTrip "D7"
    test "F#7" do  
      assertRoundTrip "F#7" 
    test "F#m7" do  
      assertRoundTrip "F#m7"
    test "Gm7" do  
      assertRoundTrip "Gm7"
    test "G#m7" do  
      assertRoundTrip "G#m7"

normaliseSuite :: Free TestF Unit
normaliseSuite =
  suite "chord symbols requiring simple substitution normalisation" do       
    test "A major" do  
      assertCanonical "A" "Amaj"      
    test "B minor" do  
      assertCanonical "Bm" "Bmin"

enharmonicEquivalenceSuite :: Free TestF Unit
enharmonicEquivalenceSuite =       
  suite "enharmonic equivalence" do    
    test "Bb major" do  
      assertCanonical "A#" "Bb" 
    test "Eb minor" do  
      assertCanonical "D#m" "Ebm"
    test "Ab diminished" do  
      assertCanonical "G#dim" "Abdim"
    test "Bb augmented" do  
      assertCanonical "A#+" "Bb+"
    test "Eb7" do  
      assertCanonical "D#7" "Eb7"
    test "Abm7" do  
      assertCanonical "G#m7" "Abm7"     
    test "Bb minor" do  
      assertCanonical "A#m" "Bbmin"    


ignoredSlashNoteSuite :: Free TestF Unit
ignoredSlashNoteSuite =         
  suite "ignored slash note" do     
    test "C/G" do  
      assertCanonical "C" "C/G"             