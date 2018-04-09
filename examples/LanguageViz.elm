module LanguageViz exposing (..)

import Color as C
import Diagrams.Align exposing (..)
import Diagrams.Core exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)
import Diagrams.Type exposing (..)
import Element as E
import Html exposing (Html)
import List as L
import Mouse
import Task
import Text as T
import Window


-- EXPR MODEL


type Expr
    = Ap Expr (List Expr)
    | IfExpr Expr Expr Expr
    | Variable String
    | LetExpr (List ( String, Expr )) Expr
      -- literals
    | DataConstructor -- ...
    | IntLit Int
    | StringLit String
    | FloatLit Float



-- STYLES


defTextStyle =
    let
        ds =
            T.defaultStyle
    in
    { ds
        | typeface =
            [ "Courier"
            , "Courier New"
            , "Lucida Console"
            , "Monaco"
            , "Consolas"
            ]
    }


numberLitStyle =
    { defTextStyle | color = C.blue }


intLitStyle =
    numberLitStyle


floatLitStyle =
    numberLitStyle


stringLitStyle =
    { defTextStyle | color = C.green }


varStyle =
    defTextStyle


keywordStyle =
    { defTextStyle
        | color = C.orange
        , bold = True
    }


hSpacer =
    hspace 7


keyword : String -> Diagram t a
keyword kw =
    text keywordStyle kw


varDia : String -> Diagram t a
varDia v =
    text varStyle v



-- VIEW


diagram : Expr -> Diagram t a
diagram expr =
    case expr of
        IntLit x ->
            text intLitStyle (toString x)

        FloatLit x ->
            text floatLitStyle (toString x)

        StringLit s ->
            text stringLitStyle ("\"" ++ s ++ "\"")

        Variable name ->
            varDia name

        Ap func args ->
            let
                comma =
                    text defTextStyle ","

                argsviews =
                    L.map diagram args

                allArgs =
                    hcat <| L.intersperse comma argsviews
            in
            hcat
                [ diagram func
                , text defTextStyle "("
                , allArgs
                , text defTextStyle ")"
                ]

        IfExpr cond tbranch fbranch ->
            vcatA LeftA
                [ hcat [ keyword "if", hSpacer, diagram cond ]
                , hcat [ keyword "then", hSpacer, diagram tbranch ]
                , hcat [ keyword "else", hSpacer, diagram fbranch ]
                ]

        LetExpr bindings expr ->
            let
                eq =
                    keyword "="

                binding ( name, exp ) =
                    hcat [ varDia name, hSpacer, eq, hSpacer, diagram exp ]

                bindingDias =
                    L.map binding bindings
            in
            vcatA LeftA
                [ hcat [ keyword "let", hSpacer, vcat bindingDias ]
                , hcat [ keyword "in", hSpacer, diagram expr ]
                ]

        _ ->
            Debug.crash "TODO"



-- TEST DATA


expr =
    LetExpr
        [ ( "foo", IntLit 62 )
        , ( "bar", StringLit "Elm is cool" )
        , ( "baz", IntLit 57 )
        ]
        (IfExpr
            (Ap (Variable "foo") [ IntLit 2, Variable "x" ])
            (StringLit "yes")
            (StringLit "no")
        )



-- INVOCATION


dia =
    alignCenter <| diagram expr


main : Program Never Model Msg
main =
    fullWindowMain dia
