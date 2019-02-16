module First.Main where

import Effect (Effect)
import First.Component as First
import Halogen.Aff as HA
import Halogen.VDom.Driver as Driver
import Prelude

-- | The top level of a halogen app
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  Driver.runUI First.component unit body
