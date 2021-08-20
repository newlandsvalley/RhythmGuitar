module RhythmGuitar.Network
  ( loadChordShapes
  , loadDefaultChordShapes
  ) where

import Prelude

import Affjax (defaultRequest, request)
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Effect.Console (log)
import Effect.Class (liftEffect)
import RhythmGuitar.Types (ChordShapes)
import RhythmGuitar.Serialization.Json (readChords)

defaultChordsJsonUri :: String
defaultChordsJsonUri = "https://raw.githubusercontent.com/newlandsvalley/RhythmGuitar/main/guitar-chords.json"

loadDefaultChordShapes :: Aff ChordShapes
loadDefaultChordShapes =
  loadChordShapes defaultChordsJsonUri

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
