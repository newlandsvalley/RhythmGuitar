module RhythmGuitar.Types where

import Data.Map (Map)

type MidiPitch = Int

type Pitches = Array MidiPitch

-- | The Chord Map defined as a mapping between chord symbol and MIDI pitches
type MidiPitchChordMap = Map String Pitches

-- | A chord shape of a guitar
type ChordShape =
  { name :: String -- the chord name
  , pitches :: Pitches -- the MIDI pitches for each String
  }

-- | The set of chord shapes that we load from the JSON file
type ChordShapes = Array ChordShape

-- | Configuration of the more static parameters for a MIDI-pitched note within a chord
type MidiChordConfig =
  { channel :: Int -- the MIDI channel
  , gain :: Number -- the volume (between 0 and 1)
  }

-- | a PSoM definition of a Pitch
type PSPitch =
  { pitchClass :: String
  , octave :: Int
  }

type PSPitches = Array PSPitch

-- | The Chord Map defined as a mapping between chord symbol and PSoM pitches
type PSPitchChordMap = Map String PSPitches

-- | default settings for MIDI chord definitions for properties that are usually static
defaultMidiChordConfig :: MidiChordConfig
defaultMidiChordConfig =
  { channel: 1
  , gain: 0.5
  }
