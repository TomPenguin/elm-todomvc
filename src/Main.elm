module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events as Events
import Html.Keyed as Keyed


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
    | AddTodo
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

        AddTodo ->
            ( { model | todos = addedTodos model.todos model.input, input = "" }, Cmd.none )

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
            [ section [ style "margin" "4rem auto", style "width" "30rem" ]
                [ h1
                    [ style "text-align" "center"
                    , style "font-weight" "100"
                    , style "font-size" "6rem"
                    , style "color" "rgba(175, 47, 47, 0.15)"
                    ]
                    [ text "todos" ]
                , section []
                    [ div []
                        [ input [ value model.input, Events.onInput Input ] []
                        , button [ Events.onClick AddTodo ] [ text "add" ]
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
