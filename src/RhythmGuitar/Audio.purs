module RhythmGuitar.Audio
  ( buildMidiChordMap
  , buildPSChordMap
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

-- | build a map from chord symbol to MIDI note array
buildMidiChordMap :: ChordShapes -> MidiPitchChordMap
buildMidiChordMap chordShapes =
  fromFoldable tuples

  where
  tuples = map (\shape -> Tuple shape.name shape.pitches) chordShapes

-- | build a map from chord symbol to PSoM note array
buildPSChordMap :: ChordShapes -> PSPitchChordMap
buildPSChordMap chordShapes =
  fromFoldable tuples

  where
  tuples = map (\shape -> Tuple shape.name (map toPSPitch shape.pitches)) chordShapes

-- | Play a chord symbol using an instrument selected from the array whose position 
-- | in the array matches the MIDI channel.  This uses the MIDI note chord map.
playChordSymbol :: Array Instrument -> String -> MidiChordConfig -> Number -> MidiPitchChordMap -> Effect Number
playChordSymbol instruments chordSym config duration midiPitchMap = do
  case (lookup (normalise chordSym) midiPitchMap) of
    Just pitches -> do
      let
        _ = spy "playing chord" chordSym
        chord = map (makeMidiNote config duration) pitches
      playNotes instruments chord
    _ -> do
      let
        _ = spy "nothing to play" chordSym
      pure 0.0

-- make a MIDINote defintion for use in a MIDI chord
makeMidiNote :: MidiChordConfig -> Number -> MidiPitch -> MidiNote
makeMidiNote config duration pitch =
  { channel: config.channel -- the MIDI channel
  , id: pitch -- the MIDI pitch number
  , timeOffset: 0.0 -- the time delay in seconds before the note is played
  , duration: duration -- the duration of the note
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