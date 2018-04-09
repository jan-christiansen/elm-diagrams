module Diagrams.FullWindow exposing (..)

{-| Utilities for when you just want to get a diagram on the whole screen.

See `Diagrams.Wiring` docs for more info on `CollageLocation`s.

@docs fullWindowCollageLocFunc, fullWindowCollageLoc, fullWindowUpdates, fullWindowMain, fullWindowView

-}

import Collage as C
import Diagrams.Core exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Wiring exposing (..)
import Element as E
import Html exposing (Html)
import Task
import Window


{-| A location function which always returns a `CollageLocation` in the middle of the window,
filling the whole window.
-}
fullWindowCollageLocFunc : CollageLocFunc
fullWindowCollageLocFunc dims =
    { offset = ( 0.0, 0.0 ), dims = dims }



-- {-| Signal of full-window collage locations, updating as the window size changes.
-- -}
-- fullWindowCollageLoc : Signal CollageLocation
-- fullWindowCollageLoc =
--     S.map fullWindowCollageLocFunc floatWindowDims
-- {-| Signal of location and mouse updates for when diagram is filling the whole screen.
-- -}
-- fullWindowUpdates : Signal ( CollageLocation, PrimMouseEvent )
-- fullWindowUpdates =
--     makeUpdateStream fullWindowCollageLocFunc
-- {-| The easiest way to get a diagram on the screen:
--
--     main =
--         fullWindowMain (rect 10 10 (justFill <| Solid Color.orange))
--
-- -}
-- fullWindowMain : Diagram t a -> Signal E.Element
-- fullWindowMain dia =
--     S.map (\dims -> fullWindowView dims dia) Window.dimensions


type Msg
    = Resize Window.Size


type Model
    = NoSize
    | Size Window.Size


initModel : ( Model, Cmd Msg )
initModel =
    ( NoSize, Task.perform Resize Window.size )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            ( Size size, Cmd.none )


view : Diagram t a -> Model -> Html msg
view diagram model =
    case model of
        NoSize ->
            Html.text "Waiting for size of window"

        Size { width, height } ->
            E.toHtml <|
                fullWindowView ( width, height ) diagram


fullWindowMain : Diagram t a -> Program Never Model Msg
fullWindowMain diagram =
    Html.program
        { init = initModel
        , subscriptions = \_ -> Window.resizes Resize
        , view = view diagram
        , update = update
        }


{-| -}
fullWindowView : ( Int, Int ) -> Diagram t a -> E.Element
fullWindowView ( w, h ) d =
    C.collage w h [ render d ]
