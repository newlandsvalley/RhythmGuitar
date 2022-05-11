module Test.Main where

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Prelude ((<>), ($), Unit, discard, show)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)
import RhythmGuitar.Normalise (parse)

assertRoundTrip :: String  -> Aff Unit
assertRoundTrip s  =
  case (parse s) of
    Right chordSym ->
      s `shouldEqual` chordSym

    Left err ->
      fail ("parse failed: " <> (show err))
      
assertCanonical :: String -> String -> Aff Unit
assertCanonical canonical s  =
  case (parse s) of
    Right chordSym ->
      canonical `shouldEqual` chordSym

    Left err ->
      fail ("parse failed: " <> (show err))

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter] do
  describe "rhythm guitar" do
    identitySpec
    normaliseSpec
    enharmonicEquivalenceSpec
    ignoredSlashNoteSpec

identitySpec :: Spec Unit
identitySpec =
  describe "chord symbols requiring no normalisation" do
    it "recognizes A major" do  
      assertRoundTrip "A"
    it "recognizes C# major" do  
      assertRoundTrip "C#"
    it "recognizes D minor" do  
      assertRoundTrip "Dm"
    it "recognizes F# minor" do  
      assertRoundTrip "F#m"
    it "recognizes G diminished" do  
      assertRoundTrip "Gdim7"
    it "recognizes G# diminished" do  
      assertRoundTrip "G#dim7"
    it "recognizes A augmented" do  
      assertRoundTrip "A+"
    it "recognizes C suspended" do  
      assertRoundTrip "Csus"
    it "recognizes C# augmented" do  
      assertRoundTrip "C#+"
    it "recognizes D7" do  
      assertRoundTrip "D7"
    it "recognizes F#7" do  
      assertRoundTrip "F#7" 
    it "recognizes F#m7" do  
      assertRoundTrip "F#m7"
    it "recognizes Gm7" do  
      assertRoundTrip "Gm7"
    it "recognizes G#m7" do  
      assertRoundTrip "G#m7"
    it "recognizes GM7" do  
      assertRoundTrip "GM7"

normaliseSpec :: Spec Unit
normaliseSpec =
  describe "chord symbols requiring simple substitution normalisation" do       
    it "normalizes A major" do  
      assertCanonical "A" "Amaj"      
    it "normalizes B minor" do  
      assertCanonical "Bm" "Bmin"    
    it "normalizes B major seventh" do  
      assertCanonical "BM7" "Bmaj7"   
    it "normalizes B Major seventh" do  
      assertCanonical "BM7" "BMaj7"

enharmonicEquivalenceSpec :: Spec Unit
enharmonicEquivalenceSpec =       
  describe "enharmonic equivalence" do    
    it "to Bb major" do  
      assertCanonical "A#" "Bb" 
    it "to Eb minor" do  
      assertCanonical "D#m" "Ebm"
    it "to Ab diminished seventh" do  
      assertCanonical "G#dim7" "Abdim7"
    it "to Bb augmented" do  
      assertCanonical "A#+" "Bb+"
    it "to Eb7" do  
      assertCanonical "D#7" "Eb7"
    it "to Abm7" do  
      assertCanonical "G#m7" "Abm7"     
    it "to Bb minor" do  
      assertCanonical "A#m" "Bbmin"    

ignoredSlashNoteSpec :: Spec Unit
ignoredSlashNoteSpec =         
  describe "ignored slash note" do     
    it "ignores G in C/G" do  
      assertCanonical "C" "C/G"             