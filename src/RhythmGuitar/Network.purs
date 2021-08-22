module RhythmGuitar.Network
  ( loadChordShapes
  , loadDefaultChordShapes
  ) where

import Prelude

import Affjax (defaultRequest, request)
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Class (liftEffect)
import RhythmGuitar.Types (ChordShapes)
import RhythmGuitar.Serialization.Json (readChords)

-- | The default JSON representation of guitar chords is held at this URI
defaultChordsJsonUri :: String
defaultChordsJsonUri = "https://raw.githubusercontent.com/newlandsvalley/RhythmGuitar/main/guitar-chords.json"

-- | load the chord shape definitions from the default location
loadDefaultChordShapes :: Aff ChordShapes
loadDefaultChordShapes =
  loadChordShapes defaultChordsJsonUri

-- | load the chord shape definitions from the requested location
loadChordShapes :: String -> Aff ChordShapes
loadChordShapes url = do
  res <- request $ defaultRequest
    { url = url, method = Left GET, responseFormat = ResponseFormat.string }

  case res <#> _.body of
    Left _ -> do
      _ <- liftEffect $ log $ "chord shapes failed to load: " <> url
      pure []
    Right body -> do
      let
        ejson = readChords body
        chordShapes = either (const []) identity ejson
      pure chordShapes
