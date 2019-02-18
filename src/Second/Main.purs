module Second.Main where

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as Driver
import Prelude
import Second.Component as Second

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  Driver.runUI Second.component unit body
