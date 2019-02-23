module Fourth.Main where

import Effect (Effect)
import Fourth.Page as Page
import Halogen.Aff as HA
import Halogen.VDom.Driver as Driver
import Prelude

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  Driver.runUI Page.component unit body
  
