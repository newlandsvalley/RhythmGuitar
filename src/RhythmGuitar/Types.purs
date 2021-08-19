module RhythmGuitar.Types where


type MidiPitch = Int

type Pitches = Array MidiPitch

-- | A chord shape of a guitar
type ChordShape =
  { name :: String             -- the chord name
  , pitches :: Pitches         -- the piches for each String
  }

type ChordShapes = Array ChordShape