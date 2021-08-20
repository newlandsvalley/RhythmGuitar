module RhythmGuitar.Types where

import Data.Map (Map)
import Audio.SoundFont (MidiNote)

type MidiPitch = Int

type Pitches = Array MidiPitch

-- | A chord shape of a guitar
type ChordShape =
  { name :: String -- the chord name
  , pitches :: Pitches -- the piches for each String
  }

type ChordShapes = Array ChordShape

-- | Configuration of a note within a chord
type Config =
  { channel :: Int -- the MIDI channel
  , duration :: Number -- the duration of the note
  , gain :: Number -- the volume (between 0 and 1)
  }

defaultConfig :: Config
defaultConfig =
  { channel: 1
  , duration: 1.5
  , gain: 0.5
  }

type ChordMap = Map String (Array MidiNote)