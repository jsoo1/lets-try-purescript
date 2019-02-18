module Third.Main where

import Effect (Effect)
import Third.Github as Github
import Halogen.Aff as HA
import Halogen.VDom.Driver as Driver
import Prelude

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  Driver.runUI Github.component unit body
  
