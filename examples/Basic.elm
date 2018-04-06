module Basic exposing (..)

-- whoooo all the moduless

import Collage as C
import Color
import Diagrams.Actions exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Core exposing (..)
import Diagrams.Debug exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Interact exposing (..)
import Diagrams.Pad exposing (..)
import Diagrams.Query exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Wiring exposing (..)
import Element as E
import Html exposing (Html)
import List as L
import Text as T
import Window


type Tag
    = RectOrange
    | RectBlue
    | Circ
    | Textt


type Action
    = ClickCirc Point
    | EnterOrange Point
    | LeaveOrange Point
    | MoveBlue Point


defLine =
    C.defaultLine


testDia : Diagram Tag Action
testDia =
    let
        aPath =
            path [ ( -50, -50 ), ( 30, 100 ) ] C.defaultLine

        rectOrange =
            tagWithActions RectOrange
                { emptyActionSet
                    | mouseEnter = Just <| keepBubbling <| \(MouseEvent evt) -> [ EnterOrange evt.offset ]
                    , mouseLeave = Just <| keepBubbling <| \(MouseEvent evt) -> [ LeaveOrange evt.offset ]
                }
            <|
                rect 50 70 (fillAndStroke (Solid Color.orange) { defLine | width = 20, cap = C.Padded })

        rectBlue =
            tagWithActions RectBlue
                { emptyActionSet | mouseMove = Just <| keepBubbling <| \(MouseEvent evt) -> [ MoveBlue evt.offset ] }
            <|
                rect 70 50 (justSolidFill Color.blue)

        rects =
            vcat [ rectOrange, rectBlue ]

        circ =
            tagWithActions Circ
                { emptyActionSet | click = Just <| keepBubbling <| \(MouseEvent evt) -> [ ClickCirc evt.offset ] }
            <|
                circle 20 (fillAndStroke (Solid Color.yellow) { defLine | width = 2, cap = C.Padded })

        justText =
            text
                (let
                    ds =
                        T.defaultStyle
                 in
                 { ds | bold = True }
                )
                "Foo"

        someText =
            tag Textt <| background (justSolidFill Color.lightBlue) <| pad 5 <| justText

        stuff =
            atop circ (above rectOrange (beside rectBlue (above circ someText)))

        moreStuff =
            hcat <| L.intersperse circ (L.repeat 5 rectOrange)
    in
    showOrigin <| showBBox <| alignCenter <| above stuff (above stuff moreStuff)


type alias Model =
    ()


initModel : Model
initModel =
    ()


update : msg -> Model -> Model
update msg model =
    model


view : Model -> Html msg
view model =
    E.toHtml (fullWindowView ( 400, 400 ) testDia)



-- case model of
--     Size dims ->
--         E.toHtml <| fullWindowView dims testDia


main : Program Never Model msg
main =
    -- Html.program
    --     { init = initModel
    --     , subscriptions = Window.resizes Resize
    --     , view = view
    --     , update = update
    --     }
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }



-- diagrams : Signal (Diagram Tag Action)
-- diagrams =
--     interactFold updateF renderF fullWindowCollageLocFunc initModel
-- main =
--     Signal.map2
--         (\dims diagram ->
--             Html.fromElement <| fullWindowView dims diagram
--         )
--         Window.dimensions
--         diagrams
