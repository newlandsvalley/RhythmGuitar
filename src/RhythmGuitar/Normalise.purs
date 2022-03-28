module RhythmGuitar.Normalise
  ( normalise
  , parse
  ) where

import Control.Alt ((<|>))
import Data.Either (Either, either)
import Data.String (length)
import Prelude ((<>), (<$), (<$>), (<*>), (<*), (&&), (==), ($), const, identity)
import StringParser (Parser, ParseError, runParser)
import StringParser.CodePoints (string, regex)
import StringParser.Combinators (choice, option, optionMaybe)

-- | Entry point - normalise a chord symbol to its canonical form
-- | if it fails to parse, give back the original
normalise :: String -> String
normalise s =
  either (const s) identity (parse s)

-- | Parse a chord symbol
parse :: String -> Either ParseError String
parse s =
  runParser chordSymbol s

chordSymbol :: Parser String
chordSymbol =
  buildChordSymbol <$> rootNote <*> quality <*> extended <* optionalSlashNote

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
  
  option "" $ choice 
    [ majorSeventh    -- must come before major because of ambiguity
    , major 
    , minor
    , diminished
    , augmented
    , suspended
    ]

majorSeventh :: Parser String
majorSeventh =
  "M7" <$ (string "M7" <|> string "maj7"<|> string "Maj7")

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

suspended :: Parser String
suspended =
  string "sus"

extended :: Parser String
extended =
  option "" (regex "[79]")

-- we throw away any slashed base note after the chord symbol proper
optionalSlashNote :: Parser String
optionalSlashNote =
  "" <$ optionMaybe slashNote

slashNote :: Parser String
slashNote =
  (<>) <$> string "/" <*> rootNote

buildChordSymbol :: String -> String -> String -> String
buildChordSymbol root qual extension =
  root <> qual <> fullExtension

  where
  -- ensure we label chord symbols marked merely as dim to be dim7
  fullExtension =
    if (qual == "dim") && (length extension == 0) then
      "7"
    else
      extension

enharmonicEquivalence :: String -> String
enharmonicEquivalence = case _ of
  "Bb" -> "A#"
  "Db" -> "C#"
  "Eb" -> "D#"
  "Gb" -> "F#"
  "Ab" -> "G#"
  x -> x

