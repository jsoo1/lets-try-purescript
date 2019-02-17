module Third.HTML (btn) where


import Halogen.HTML as HH
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Elements (Node)
import DOM.HTML.Indexed (HTMLbutton)
import Prelude


btn :: forall p i. Node HTMLbutton p i
btn properties =
  HH.button $ [ class_ $ HH.ClassName "btn" ] <> properties
