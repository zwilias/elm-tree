module TwoThree exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tree.TwoThree as Tree exposing (Tree)
import Json.Decode as Decode
import Keyboard


main : Program Never Model Msg
main =
    Html.program
        { update = update
        , view = view
        , init = init
        , subscriptions = subscriptions
        }


shiftKey : Keyboard.KeyCode
shiftKey =
    16


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs
            (\x ->
                if x == shiftKey then
                    PressShift
                else
                    NoOp
            )
        , Keyboard.ups
            (\x ->
                if x == shiftKey then
                    ReleaseShift
                else
                    NoOp
            )
        ]



-- MODEL


type alias Model =
    { nodeEnty : String
    , set : Tree String
    , shiftPressed : Bool
    }


model : Tree String -> Model
model tree =
    Model "" tree False


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
    = NoOp
    | ChangeInput String
    | InsertNode
    | DeleteNode String
    | DeleteNodeOrNode String String
    | PressShift
    | ReleaseShift


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NoOp ->
            model ! []

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

        DeleteNode item ->
            { model
                | set = Tree.remove item model.set
            }
                ! []

        DeleteNodeOrNode noShift withShift ->
            let
                item : String
                item =
                    if model.shiftPressed then
                        withShift
                    else
                        noShift
            in
                { model
                    | set = Tree.remove item model.set
                }
                    ! []

        PressShift ->
            { model | shiftPressed = True } ! []

        ReleaseShift ->
            { model | shiftPressed = False } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "width", "600px" )
            , ( "margin", "0 auto" )
            ]
        ]
        [ p [] [ text "Click nodes to remove their first value. On nodes with multiple values (3-nodes), shift-click to remove the second value." ]
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
renderTree set =
    case set of
        Tree.Empty ->
            text "."

        Tree.TwoNode lower item greater ->
            div
                [ style
                    [ ( "border", "1px solid grey" )
                    , ( "padding", "5px" )
                    , ( "margin", "5px" )
                    , ( "border-radius", "5px" )
                    , ( "background-color", "rgba(0, 0, 0, 0.05)" )
                    ]
                , onClickNoBubble <| DeleteNode item
                ]
                [ div []
                    [ text item
                    ]
                , renderSubtrees [ lower, greater ]
                ]

        Tree.ThreeNode lower left middle right greater ->
            div
                [ style
                    [ ( "border", "1px solid grey" )
                    , ( "padding", "5px" )
                    , ( "margin", "5px" )
                    , ( "border-radius", "5px" )
                    , ( "background-color", "rgba(0, 0, 0, 0.05)" )
                    ]
                , onClickNoBubble <| DeleteNodeOrNode left right
                ]
                [ div []
                    [ text left
                    , text " - "
                    , text right
                    ]
                , renderSubtrees [ lower, middle, greater ]
                ]


renderSubtrees : List (Tree String) -> Html Msg
renderSubtrees subtrees =
    let
        renderSubtree : Tree String -> Html Msg
        renderSubtree tree =
            div [ style [ ( "flex-grow", "1" ) ] ] [ renderTree tree ]
    in
        div
            [ style
                [ ( "display", "flex" )
                ]
            ]
            (List.map
                renderSubtree
                subtrees
            )



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
