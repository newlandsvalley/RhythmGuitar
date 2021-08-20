module Example.Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import PlayerComponent as Player

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Player.component unit body

