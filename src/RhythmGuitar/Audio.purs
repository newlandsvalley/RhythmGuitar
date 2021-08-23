module RhythmGuitar.Audio
  ( buildMidiChordMap
  , buildMidiChord
  , buildPSChordMap
  , lookupChordMidiPitches
  , lookupChordPSPitches
  , playChordSymbol
  ) where

import Audio.SoundFont (Instrument, MidiNote, playNotes)
import Effect (Effect)
import Data.Array ((!!))
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude ((/), map, mod, pure)
import RhythmGuitar.Types
import RhythmGuitar.Normalise (normalise)
import Data.Tuple (Tuple(..))

import Debug (spy)

-- | Build a map from chord symbol to MIDI note array
buildMidiChordMap :: ChordShapes -> MidiPitchChordMap
buildMidiChordMap chordShapes =
  fromFoldable tuples

  where
  tuples = map (\shape -> Tuple shape.name shape.pitches) chordShapes

-- | Build a MIDI chord, suitable for a soundfont player
buildMidiChord :: MidiChordConfig -> Pitches -> Array MidiNote
buildMidiChord config pitches =
  map (buildMidiNote config) pitches

-- | Build a map from chord symbol to PSoM note array
buildPSChordMap :: ChordShapes -> PSPitchChordMap
buildPSChordMap chordShapes =
  fromFoldable tuples

  where
  tuples = map (\shape -> Tuple shape.name (map toPSPitch shape.pitches)) chordShapes

-- | Look up the MIDI pitches that make up the chord (if it exists in the map) 
lookupChordMidiPitches :: String -> MidiPitchChordMap -> (Maybe Pitches)
lookupChordMidiPitches chordSym midiPitchMap =
  lookup (normalise chordSym) midiPitchMap

-- | Look up the PSoM pitches that make up the chord (if it exists in the map) 
lookupChordPSPitches :: String -> PSPitchChordMap -> (Maybe PSPitches)
lookupChordPSPitches chordSym psPitchMap =
  lookup (normalise chordSym) psPitchMap

-- | Play a chord symbol using an instrument selected from the array whose position 
-- | in the array matches the MIDI channel.  This uses the MIDI note chord map.
playChordSymbol :: Array Instrument -> String -> MidiChordConfig -> MidiPitchChordMap -> Effect Number
playChordSymbol instruments chordSym config midiPitchMap = do
  case (lookupChordMidiPitches chordSym midiPitchMap) of
    Just pitches -> do
      let
        _ = spy "playing chord" chordSym
        chord = buildMidiChord config pitches
      playNotes instruments chord
    _ -> do
      let
        _ = spy "nothing to play" chordSym
      pure 0.0

-- build a MIDINote defintion for use in a MIDI chord
buildMidiNote :: MidiChordConfig -> MidiPitch -> MidiNote
buildMidiNote config pitch =
  { channel: config.channel -- the MIDI channel
  , id: pitch -- the MIDI pitch number
  , timeOffset: config.timeOffset -- the time delay in seconds before the note is played
  , duration: config.duration -- the duration of the note
  , gain: config.gain -- the volume (between 0 and 1)
  }

-- translate a MIDI pitch to a PSoM pitch
toPSPitch :: MidiPitch -> PSPitch
toPSPitch mp =
  let
    octave = mp / 12
    n = mp `mod` 12
    pitchClass = fromMaybe "C" ([ "C", "Cs", "D", "Ds", "E", "F", "Fs", "G", "Gs", "A", "As", "B" ] !! n)
  in
    { pitchClass, octave }