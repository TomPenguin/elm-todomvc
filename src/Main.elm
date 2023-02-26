module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (autofocus, placeholder, style, value)
import Html.Events as Events exposing (keyCode)
import Html.Keyed as Keyed
import Json.Decode


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Todo =
    { text : String
    , completed : Bool
    }


type alias Model =
    { input : String
    , todos : List Todo
    }


type alias Text =
    String


type alias Id =
    Int


type Tab
    = AllTab
    | ActiveTab
    | CompletedTab


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" [], Cmd.none )


type Msg
    = Input String
    | KeyPress Int
    | ToggleTodo Int



-- | UpdateTodo Text Id
-- | DeleteTodo Id
-- | ClearCompleted
-- | SwitchTab Tab


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input text ->
            ( { model | input = text }, Cmd.none )

        KeyPress keyCode ->
            case keyCode of
                13 ->
                    ( { model | todos = addedTodos model.todos model.input, input = "" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleTodo id ->
            ( { model | todos = toggleTodo model.todos id }, Cmd.none )


addedTodos : List Todo -> Text -> List Todo
addedTodos todos text =
    List.append todos [ Todo text False ]


toggleTodo : List Todo -> Id -> List Todo
toggleTodo todos id =
    todos
        |> List.indexedMap
            (\index todo ->
                if index == id then
                    { todo | completed = not todo.completed }

                else
                    todo
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm・TodoMVC"
    , body =
        [ main_ []
            [ section [ style "margin" "4rem auto", style "width" "36rem" ]
                [ h1
                    [ style "text-align" "center"
                    , style "font-weight" "100"
                    , style "font-size" "6rem"
                    , style "color" "rgba(175, 47, 47, 0.15)"
                    ]
                    [ text "todos" ]
                , section
                    [ style "background" "#fff"
                    , style "box-shadow" "0 2px 4px 0 rgb(0 0 0 / 20%), 0 25px 50px 0 rgb(0 0 0 / 10%)"
                    ]
                    [ div []
                        [ input
                            [ value model.input
                            , placeholder "What needs to be done?"
                            , autofocus True
                            , Events.onInput Input
                            , onKeyPress KeyPress
                            ]
                            []
                        ]
                    , viewTodos model.todos
                    ]
                ]
            ]
        ]
    }


viewTodos : List Todo -> Html Msg
viewTodos todos =
    Keyed.node "ul" [] (List.indexedMap viewTodo todos)


viewTodo : Int -> Todo -> ( String, Html Msg )
viewTodo idx todo =
    ( String.fromInt idx
    , li []
        [ viewTodoText todo
        , button [ Events.onClick (ToggleTodo idx) ]
            [ text
                (if todo.completed then
                    "incomplete"

                 else
                    "complete"
                )
            ]
        ]
    )


viewTodoText : Todo -> Html Msg
viewTodoText todo =
    if todo.completed then
        p [ style "text-decoration" "line-through" ] [ text todo.text ]

    else
        p [] [ text todo.text ]


onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
    Events.on "keypress" <| Json.Decode.map tagger Events.keyCode
