module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
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



-- MODEL


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



-- UPDATE


type Msg
    = Input String
    | KeyPress Int
    | ToggleTodo Int
    | DeleteTodo Int


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

        DeleteTodo id ->
            ( { model | todos = deletedTodos model.todos id }, Cmd.none )


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


deletedTodos : List Todo -> Int -> List Todo
deletedTodos todos id =
    todos
        |> List.indexedMap Tuple.pair
        |> List.filter (\( idx, _ ) -> idx /= id)
        |> List.map Tuple.second


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm・TodoMVC"
    , body =
        [ main_ []
            [ section [ Attr.class "container" ]
                [ h1 [ Attr.class "heading" ] [ text "todos" ]
                , section
                    [ Attr.class "card" ]
                    [ viewHeader model.input
                    , viewTodos model.todos
                    , viewFooter model.todos
                    ]
                ]
            ]
        ]
    }


viewHeader : String -> Html Msg
viewHeader inputText =
    header []
        [ input
            [ Attr.class "input"
            , Attr.value inputText
            , Attr.placeholder "What needs to be done?"
            , Attr.autofocus True
            , Events.onInput Input
            , onKeyPress KeyPress
            ]
            []
        ]


viewTodos : List Todo -> Html Msg
viewTodos todos =
    Keyed.node "ul" [ Attr.class "todos" ] (List.indexedMap viewTodo todos)


viewTodo : Int -> Todo -> ( String, Html Msg )
viewTodo idx todo =
    ( String.fromInt idx
    , li [ Attr.class "todos__todo" ]
        [ label []
            [ input
                [ Attr.type_ "checkbox"
                , Attr.checked todo.completed
                , Events.onClick (ToggleTodo idx)
                ]
                []
            , p
                (if todo.completed then
                    [ Attr.class "todos__todo", Attr.class "todos__todo--completed" ]

                 else
                    [ Attr.class "todos__todo" ]
                )
                [ text todo.text ]
            ]
        , button [ Attr.class "delete", Events.onClick (DeleteTodo idx) ] []
        ]
    )


viewFooter : List Todo -> Html Msg
viewFooter todos =
    footer []
        [ span []
            [ text
                ((todos
                    |> List.filter (\todo -> not todo.completed)
                    |> List.length
                    |> String.fromInt
                 )
                    ++ " items left"
                )
            ]
        ]


onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
    Events.on "keypress" <| Json.Decode.map tagger Events.keyCode
