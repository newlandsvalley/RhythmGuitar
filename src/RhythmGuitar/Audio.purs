module RhythmGuitar.Audio
  ( buildChordMap
  , buildDefaultChordMap
  , playChordSymbol
  ) where

import Audio.SoundFont (Instrument, MidiNote, playNotes)
import Effect (Effect)
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Prelude (map, pure)
import RhythmGuitar.Types
import RhythmGuitar.Normalise (normalise)
import Data.Tuple (Tuple(..))

import Debug (spy)

buildDefaultChordMap :: ChordShapes -> ChordMap
buildDefaultChordMap chordShapes =
  buildChordMap defaultConfig chordShapes

buildChordMap :: Config -> ChordShapes -> ChordMap
buildChordMap config chordShapes =
  fromFoldable tuples

  where
  builder :: ChordShape -> Tuple String (Array MidiNote)
  builder chordShape =
    Tuple chordShape.name (makeChord config chordShape.pitches)
  tuples = map builder chordShapes

makeChord :: Config -> Pitches -> Array MidiNote
makeChord config pitches =
  map makeNote pitches

  where
  makeNote :: MidiPitch -> MidiNote
  makeNote pitch =
    { channel: config.channel -- the MIDI channel
    , id: pitch -- the MIDI pitch number
    , timeOffset: 0.0 -- the time delay in seconds before the note is played
    , duration: config.duration -- the duration of the note
    , gain: config.gain -- the volume (between 0 and 1)
    }

playChordSymbol :: Array Instrument -> String -> ChordMap -> Effect Number
playChordSymbol instruments chordSym chordMap = do
  let
    mChord = lookup (normalise chordSym) chordMap
  case mChord of
    Just chord -> do
      let
        _ = spy "playing chord" chord
      playNotes instruments chord
    _ -> do
      let
        _ = spy "nothing to play" mChord
      pure 0.0