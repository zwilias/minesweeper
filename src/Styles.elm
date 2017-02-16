module Styles exposing (css, CssClass(..))

{-| Define all the styling, using rtfeldman's elm-css.

@docs css, CssClass
-}

import Css exposing (..)
import Css.Namespace exposing (namespace)


{-| Enumerate all the defined css classes, so they can be referenced
type-safely.
-}
type CssClass
    = Row
    | Cell
    | Discovered
    | Mine
    | Potential
    | Flagged



-- #FFC658


{-| Generate the stylesheet.
-}
css : Stylesheet
css =
    (stylesheet << namespace "sweeper")
        [ class Row
            [ displayFlex
            ]
        , class Cell
            [ width (px 16)
            , height (px 16)
            , border3 (px 1) solid (hex "aaa")
            , borderRadius (px 2)
            , displayFlex
            , alignItems center
            , textAlign center
            , fontFamily monospace
            , fontSize (px 20)
            , property "user-select" "none"
            ]
        , class Flagged
            [ hover
                [ backgroundColor (hex "00A")
                ]
            , backgroundColor (hex "00A")
            , borderColor (hex "333")
            , color (hex "F00")
            ]
        , class Potential
            [ hover
                [ backgroundColor (hex "ccc")
                ]
            , backgroundColor (hex "ddd")
            , borderColor (hex "333")
            ]
        , class Mine
            [ backgroundColor (hex "F00") ]
        ]
