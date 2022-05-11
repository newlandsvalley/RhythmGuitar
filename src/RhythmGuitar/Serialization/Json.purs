module RhythmGuitar.Serialization.Json where

import Data.Either (Either)
import Foreign (MultipleErrors)
import RhythmGuitar.Types as Guitar
import Yoga.JSON as JSON

writeChords :: Guitar.ChordShapes -> String
writeChords =
  JSON.writeJSON

readChords :: String -> Either MultipleErrors Guitar.ChordShapes
readChords =
  JSON.readJSON

