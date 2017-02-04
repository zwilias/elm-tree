module AA exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tree.AA as Tree exposing (Tree)
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { update = update
        , view = view
        , init = init
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { nodeEnty : String
    , set : Tree String
    }


model : Tree String -> Model
model tree =
    Model "" tree


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            "acegikm"
                |> String.toList
                |> List.map String.fromChar
                |> Tree.fromList
                |> model
    in
        initialModel ! []



-- UPDATE


type Msg
    = ChangeInput String
    | InsertNode
    | RemoveNode String


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        ChangeInput newInput ->
            { model
                | nodeEnty = newInput
            }
                ! []

        InsertNode ->
            { model
                | nodeEnty = ""
                , set = Tree.insert model.nodeEnty model.set
            }
                ! []

        RemoveNode node ->
            { model
                | set = Tree.remove node model.set
            }
                ! []



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "width", "600px" )
            , ( "margin", "0 auto" )
            ]
        ]
        [ p [] [ text "Click nodes to remove them." ]
        , renderForm model.nodeEnty
        , renderTree model.set
        ]


renderForm : String -> Html Msg
renderForm currentText =
    div
        [ style
            [ ( "border", "1px solid black" )
            , ( "padding", "10px" )
            , ( "margin", "10px 0" )
            , ( "display", "flex" )
            , ( "justify-content", "space-between" )
            ]
        ]
        [ input [ value currentText, onInput ChangeInput ] []
        , button [ onClick InsertNode ] [ text "Add" ]
        ]


renderTree : Tree String -> Html Msg
renderTree tree =
    case tree of
        Tree.Empty ->
            text "."

        Tree.Node level left head right ->
            div
                [ style
                    [ ( "border", "1px solid grey" )
                    , ( "padding", "5px" )
                    , ( "margin", "5px" )
                    , ( "border-radius", "5px" )
                    , ( "background-color", "rgba(0, 0, 0, 0.05)" )
                    ]
                , onClickNoBubble <| RemoveNode head
                ]
                [ div []
                    [ text head
                    , br [] []
                    , text <| toString level
                    ]
                , renderLeftRight left right
                ]


renderLeftRight : Tree String -> Tree String -> Html Msg
renderLeftRight left right =
    div
        [ style
            [ ( "display", "flex" )
            ]
        ]
        [ div [ style [ ( "flex-grow", "1" ) ] ] [ renderTree left ]
        , div [ style [ ( "flex-grow", "1" ) ] ] [ renderTree right ]
        ]



-- Event helpers


onClickNoBubble : msg -> Html.Attribute msg
onClickNoBubble msg =
    let
        options : Html.Events.Options
        options =
            { stopPropagation = True
            , preventDefault = True
            }
    in
        onWithOptions
            "click"
            options
            (Decode.succeed msg)
