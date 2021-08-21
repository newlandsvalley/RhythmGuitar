module PlayerComponent where

import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import Data.Foldable (traverse_)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.Midi.Instrument (InstrumentName(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Prelude (Unit, ($), bind, pure, unit)
import RhythmGuitar.Types (ChordMap)
import RhythmGuitar.Audio (buildDefaultChordMap, playChordSymbol)
import RhythmGuitar.Network (loadDefaultChordShapes)

type State =
  { instruments :: Array Instrument
  , chordMap :: ChordMap
  }

type ChildSlots :: forall k. Row k
type ChildSlots = ()

data Action
  = Initialize
  | Play

-- | The component definition
component
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , finalize = Nothing
        }
    }

  where
  initialState :: i -> State
  initialState _ =
    { instruments: []
    , chordMap: Map.empty
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render _ =
    HH.div_
      [ renderPlayButton ]

  renderPlayButton :: H.ComponentHTML Action ChildSlots m
  renderPlayButton =
    HH.div_
      [ HH.button
          [ HP.class_ $ ClassName "hoverable"
          , HE.onClick (\_ -> Play)
          ]
          [ HH.text "play" ]
      ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      -- we load piano to slot 0 (which we don't hear) and guitar to slot 1 (the config default)
      instruments <- H.liftAff $ loadRemoteSoundFonts [ AcousticGrandPiano, AcousticGuitarSteel ]
      chordShapes <- H.liftAff loadDefaultChordShapes
      let
        chordMap = buildDefaultChordMap chordShapes
      H.modify_
        ( \st -> st
            { instruments = instruments
            , chordMap = chordMap
            }
        )
    Play -> do
      state <- H.get
      _ <- H.liftAff $ playChordSequence state [ "C", "Am", "F", "G7"]
      _ <- H.liftAff $ playChordSequence state [ "C#", "A#m7", "F#", "G#7"]
      _ <- H.liftAff $ playChordSequence state [ "D", "Bm", "G", "A7"]
      _ <- H.liftAff $ playChordSequence state [ "F", "Dm7", "Bb", "C7"]
      _ <- H.liftAff $ playChordSequence state [ "Eb", "Edim7", "Fm7", "F#dim7", "Eb"]
      pure unit

playChordSequence :: forall m. MonadAff m => State -> Array String -> m Unit
playChordSequence state chordSymbols = do
  traverse_ (playChord state) chordSymbols

playChord :: forall m. MonadAff m => State -> String -> m Unit
playChord state chordSymbol = do
  _ <- liftEffect $ playChordSymbol state.instruments chordSymbol state.chordMap
  _ <- liftAff $ delay (Milliseconds 1750.0)
  pure unit

