port module Stylesheets exposing (main)

{-| Special module for taking our Styles.elm and generating an actual .css file
from it.

@docs main
-}

import Css.File exposing (CssFileStructure, CssCompilerProgram)
import Styles


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "sweeper.css", Css.File.compile [ Styles.css ] ) ]


{-| The main program - turns an elm stylesheet into whatever fileStructure
defined.
-}
main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
