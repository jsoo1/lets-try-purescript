module Third.Style ( col, row, text, cmuSerif, fontSize) where


import Prelude

import CSS ( StyleM
           , display
           , height
           , flex
           , padding
           , paddingBottom
           , paddingTop
           )
import CSS.Flexbox ( AlignItemsValue(..)
                   , alignItems
                   , column
                   , flexDirection
                   , flexBasis
                   , flexGrow
                   , flexShrink
                   )
import CSS.Font as Font
import CSS.Geometry (lineHeight)
import CSS.Property (value)
import CSS.Size (Size, pct, rem)
import Data.NonEmpty as NonEmpty


col :: StyleM Unit
col = do
  display flex
  flexDirection column
  alignItems $ AlignItemsValue $ value "center"
  height (pct 100.0)


row :: StyleM Unit
row = do
  flexBasis (pct 0.0)
  flexGrow 1
  flexShrink 1
  padding (rem 1.25) (rem 1.25) (rem 1.25) (rem 1.25)


text :: StyleM Unit
text = do
  display flex
  paddingTop (rem 1.25)
  paddingBottom (rem 1.25)


cmuSerif :: StyleM Unit
cmuSerif = do
  Font.fontFamily [ "CMUSerifRoman" ]
    $ NonEmpty.singleton Font.sansSerif


fontSize :: forall a. Size a -> StyleM Unit
fontSize size = do
  Font.fontSize size
  lineHeight size
