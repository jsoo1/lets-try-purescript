module Third.Style ( col, row, flexOne, paragraph, caption, anchor, code, text, outlined, cmuSerif, fontSize) where

import Prelude
import CSS ( StyleM
           , border
           , color
           , display
           , height
           , flex
           , padding
           , paddingBottom
           , paddingTop
           )
import CSS.Border (solid, borderRadius)
import CSS.Color (black)
import CSS.Flexbox ( AlignItemsValue(..)
                   , alignItems
                   , column
                   , flexDirection
                   , flexBasis
                   , flexGrow
                   , flexShrink
                   )
import CSS.Font (FontWeight(..))
import CSS.Font as Font
import CSS.Geometry (lineHeight)
import CSS.Property (value)
import CSS.Size (Size, pct, px, rem)
import CSS.Text (TextDecoration(..), textDecoration)
import Data.NonEmpty as NonEmpty

col :: StyleM Unit
col = do
  display flex
  flexDirection column
  alignItems $ AlignItemsValue $ value "center"
  height (pct 100.0)

row :: StyleM Unit
row = do
  flexOne
  padding (rem 1.25) (rem 1.25) (rem 1.25) (rem 1.25)

flexOne :: StyleM Unit
flexOne = do
  flexBasis (pct 0.0)
  flexGrow 1
  flexShrink 1

paragraph :: StyleM Unit
paragraph = do
  text
  cmuSerif
  fontSize (px 24.0)

caption :: StyleM Unit
caption = do
  text
  cmuSerif
  fontSize (px 18.0)

anchor :: StyleM Unit
anchor = do
  textDecoration $ TextDecoration $ value "none"
  color black

outlined :: StyleM Unit
outlined = do
  border solid (px 1.0) black
  borderRadius (px 4.0) (px 4.0) (px 4.0) (px 4.0)

code :: StyleM Unit
code = do
  inconsolata
  Font.fontWeight $ FontWeight $ value "100"
  fontSize (px 24.0)

text :: StyleM Unit
text = do
  display flex
  paddingTop (rem 1.25)
  paddingBottom (rem 1.25)

cmuSerif :: StyleM Unit
cmuSerif = do
  Font.fontFamily [ "CMUSerifRoman" ]
    $ NonEmpty.singleton Font.sansSerif

inconsolata :: StyleM Unit
inconsolata = do
  Font.fontFamily [ "Inconsolata" ]
    $ NonEmpty.singleton
    $ Font.GenericFontFamily
    $ value "monospace"

fontSize :: forall a. Size a -> StyleM Unit
fontSize size = do
  Font.fontSize size
  lineHeight size
