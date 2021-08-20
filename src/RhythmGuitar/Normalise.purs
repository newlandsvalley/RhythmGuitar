module RhythmGuitar.Normalise where


import Control.Alt ((<|>))
import Data.Either (Either)
import Prelude ((<>), (<$), (<$>), (<*>))
import Text.Parsing.StringParser (Parser, ParseError, runParser)
import Text.Parsing.StringParser.CodePoints (string, regex)
import Text.Parsing.StringParser.Combinators (option)


-- | Entry point - Parse a chord symbol
parse :: String -> Either ParseError String
parse s =
  runParser chordSymbol s

chordSymbol :: Parser String
chordSymbol =
  buildChordSymbol <$> rootNote <*> quality <*> extended

rootNote :: Parser String 
rootNote = enharmonicEquivalence <$> ((<>) <$> pitchClass <*> accidental)

pitchClass :: Parser String
pitchClass =
  regex "[A-G]"

accidental :: Parser String 
accidental =
  option "" (regex "[b#]")

quality :: Parser String 
quality =
  option "" (major <|> minor <|> diminished <|> augmented)

major :: Parser String 
major =  
  "" <$ string "maj"      

minor :: Parser String 
minor =  
  "m" <$ (string "min" <|> string "m")

diminished :: Parser String
diminished = 
  string "dim"

augmented :: Parser String
augmented = 
  string "+"      

extended :: Parser String 
extended = 
  option "" (regex "[79]")

buildChordSymbol :: String -> String -> String -> String 
buildChordSymbol root qual extension = 
  root <> qual <> extension

enharmonicEquivalence :: String -> String 
enharmonicEquivalence = case _ of 
  "Bb" -> "A#"
  "Db" -> "C#"
  "Eb" -> "D#"
  "Gb" -> "F#"
  "Ab" -> "G#"
  x -> x



