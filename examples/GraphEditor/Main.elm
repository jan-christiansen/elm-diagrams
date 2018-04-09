module GraphEditor.Main exposing (..)

import Diagrams.FullWindow as DFW
import Diagrams.Interact as DI
import Diagrams.Type exposing (Diagram)
import Diagrams.Wiring as DW
import Dict as D
import Element as E
import GraphEditor.Controller as Cont
import GraphEditor.Model exposing (Action(..), State)
import Html exposing (Html)
import Platform exposing (program)
import Task
import Window


-- DATA


fooNode =
    { title = "Foo", inPorts = [ "InAasdfasdfsdafasdfs", "asdfs", "InB", "InC" ], outPorts = [ "out1", "out2" ] }


fooPosNode =
    { node = fooNode, pos = ( -300, 100 ), id = "foo" }


bazNode =
    { title = "Baz", inPorts = [ "InA", "InB", "InC" ], outPorts = [ "out1", "out2" ] }


bazPosNode =
    { node = bazNode, pos = ( 100, -200 ), id = "baz" }


barNode =
    { title = "Bar", inPorts = [ "InA", "InB", "InC" ], outPorts = [ "out1", "out2" ] }


barPosNode =
    { node = barNode, pos = ( 100, 100 ), id = "bar" }


fooBarEdge =
    { from = ( "foo", "out1" ), to = ( "bar", "InA" ) }



--fooBazEdge = { from = ("foo", "out2"), to = ("baz", "InC") }


initGraph =
    { nodes =
        D.fromList
            [ ( fooPosNode.id, fooPosNode )
            , ( barPosNode.id, barPosNode )
            , ( bazPosNode.id, bazPosNode )
            ]
    , edges = [ fooBarEdge ]
    }


initState : State
initState =
    { graph = initGraph, dragState = Nothing, size = Nothing }



-- start 'er up
-- diagrams : Signal (Diagram Tag Action)
-- diagrams =
--     DI.interactFold Cont.update Cont.render DFW.fullWindowCollageLocFunc initState


view : State -> Html Action
view state =
    case state.size of
        Nothing ->
            Html.text "Waiting for size of window"

        Just { width, height } ->
            E.toHtml <|
                DFW.fullWindowView ( width, height ) (Cont.render state)


update : Action -> State -> ( State, Cmd Action )
update action state =
    ( Cont.update action state, Cmd.none )


main =
    Html.program
        { init = ( initState, Task.perform Resize Window.size )
        , update = update
        , subscriptions = \_ -> Window.resizes Resize
        , view = view
        }
