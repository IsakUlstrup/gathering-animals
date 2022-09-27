module View.CssExtra exposing (gap, maxWidthAnim)

import Css exposing (Style)
import Css.Animations exposing (Property)


gap : Float -> Style
gap length =
    Css.property "gap" (String.fromFloat length ++ "rem")


maxWidthAnim : Float -> Property
maxWidthAnim length =
    Css.Animations.custom "max-width" (String.fromFloat length ++ "rem")
