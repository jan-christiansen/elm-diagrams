module Tooltip exposing (..)

import Color
import Debug
import Diagrams.Align exposing (..)
import Diagrams.Core exposing (..)
import Diagrams.Debug exposing (..)
import Diagrams.Envelope exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Pad exposing (..)
import Diagrams.Type exposing (..)
import List as L
import Text as T


defText =
    T.defaultStyle


tooltip : Direction -> FillStroke -> Float -> Diagram t a -> Diagram t a
tooltip dir fs tipSpacing dia =
    let
        mainBox =
            background fs (dia |> pad 3)

        triangle =
            eqTriangle 7 fs |> rotate (directionAngle dir - pi / 2)
    in
    case dir of
        Up ->
            above triangle mainBox |> alignTop |> moveY -tipSpacing

        Down ->
            above mainBox triangle |> alignBottom |> moveY tipSpacing

        Left ->
            beside triangle mainBox |> alignLeft |> moveX tipSpacing

        Right ->
            beside mainBox triangle |> alignRight |> moveX -tipSpacing


helloWorld : Direction -> Diagram t a
helloWorld dir =
    text { defText | color = Color.white, height = Just 11 } "Hello World"
        |> tooltip dir (justSolidFill Color.black) 10
        |> showOrigin
        |> showBBox


main =
    [ Up, Right, Down, Left ]
        |> L.map (alignCenter << helloWorld)
        |> L.intersperse (hspace 10)
        |> hcat
        |> alignCenter
        |> fullWindowMain
