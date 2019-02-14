module Main where

import Prelude

import Effect (Effect)
import First as First
import Halogen.Aff as HA
import Halogen.VDom.Driver as Driver

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  Driver.runUI First.component unit body
