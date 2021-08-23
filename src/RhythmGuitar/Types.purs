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

-- | Configuration of the MIDI-pitched note within a chord other than pitch
type MidiChordConfig =
  { channel :: Int -- the MIDI channel
  , timeOffset :: Number -- the time delay in seconds before the note is played
  , duration :: Number -- the duration of the note in seconds
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

-- | default settings for MIDI chord definitions other than pitch
defaultMidiChordConfig :: MidiChordConfig
defaultMidiChordConfig =
  { channel: 1
  , timeOffset: 0.0
  , duration: 1.0
  , gain: 0.5
  }
