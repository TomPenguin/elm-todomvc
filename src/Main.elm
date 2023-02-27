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
    , activeTab : Tab
    }


type alias Text =
    String


type alias Id =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" [] AllTab, Cmd.none )



-- UPDATE


type Msg
    = Input String
    | KeyPress Int
    | ToggleTodo Int
    | DeleteTodo Int
    | SwitchTab Tab
    | ClearCompleted


type Tab
    = AllTab
    | ActiveTab
    | CompletedTab


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

        SwitchTab tab ->
            ( { model | activeTab = tab }, Cmd.none )

        ClearCompleted ->
            ( { model | todos = notCompletedTodos model.todos }, Cmd.none )


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
                    , viewTodos model
                    , viewFooter model
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


viewTodos : Model -> Html Msg
viewTodos model =
    Keyed.node "ul"
        [ Attr.class "todos" ]
        (model.todos
            |> (case model.activeTab of
                    AllTab ->
                        identity

                    ActiveTab ->
                        notCompletedTodos

                    CompletedTab ->
                        completedTodos
               )
            |> List.indexedMap viewTodo
        )


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
        , viewButton "delete" "" False (DeleteTodo idx)
        ]
    )


viewFooter : Model -> Html Msg
viewFooter model =
    footer [ Attr.class "footer" ]
        [ span []
            [ text ((notCompletedSize model.todos |> String.fromInt) ++ " items left") ]
        , span [ Attr.class "tabs" ]
            [ viewButton "tab" "All" (model.activeTab == AllTab) (SwitchTab AllTab)
            , viewButton "tab" "Active" (model.activeTab == ActiveTab) (SwitchTab ActiveTab)
            , viewButton "tab" "Completed" (model.activeTab == CompletedTab) (SwitchTab CompletedTab)
            ]
        , span []
            [ let
                label =
                    if completedSize model.todos > 0 then
                        "Clear completed(" ++ String.fromInt (completedSize model.todos) ++ ")"

                    else
                        ""
              in
              viewButton "clear" label False ClearCompleted
            ]
        ]


viewButton : String -> String -> Bool -> msg -> Html msg
viewButton class label active msg =
    button
        (if active then
            [ Attr.class "btn", Attr.class "btn--active", Attr.class class, Events.onClick msg ]

         else
            [ Attr.class "btn", Attr.class class, Events.onClick msg ]
        )
        [ text label ]


onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
    Events.on "keypress" <| Json.Decode.map tagger Events.keyCode


completedTodos : List Todo -> List Todo
completedTodos =
    List.filter .completed


notCompletedTodos : List Todo -> List Todo
notCompletedTodos =
    List.filter (.completed >> not)


completedSize : List Todo -> Int
completedSize =
    completedTodos >> List.length


notCompletedSize : List Todo -> Int
notCompletedSize todos =
    List.length todos - completedSize todos
