module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, canvas, div, h1, li, section, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Maybe exposing (Maybe(..))



-- MAIN
-- Bits inspired by https://github.com/icidasset/elm-binary


type Bits
    = Bits (List Bool)


fromIntegers : List Int -> Bits
fromIntegers =
    List.map
        (\i ->
            if i <= 0 then
                False

            else
                True
        )
        >> Bits


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type ClientType
    = Presenter
    | Participant


type ModeSelection
    = Screen
    | Window


type TransmissionMode
    = VNC
    | WebRTC


type alias Model =
    { clientType : ClientType
    , title : String
    , presentationStarted : Bool
    , showCanvas : Bool
    , showPopup : Bool
    , selection : ModeSelection
    , isMobile : Bool
    , transmissionMode : TransmissionMode
    }


init : Model
init =
    { clientType = Presenter
    , title = "Hello"
    , presentationStarted = False
    , showCanvas = False
    , showPopup = False
    , selection = Screen
    , isMobile = False
    , transmissionMode = VNC
    }



-- UPDATE


type Msg
    = StartPresentation
    | StopPresentation
    | SelectScreen
    | SelectWindow
    | ToggleClientType



--    | SelectedScreen Int
--    | SelectedWindow Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartPresentation ->
            { model | presentationStarted = True }

        StopPresentation ->
            { model | presentationStarted = False, showPopup = False }

        SelectScreen ->
            { model | selection = Screen, showPopup = True }

        SelectWindow ->
            { model | selection = Window, showPopup = True }

        ToggleClientType ->
            { model
                | clientType =
                    if model.clientType == Presenter then
                        Participant

                    else
                        Presenter
            }



-- VIEW


emptyNode : Html msg
emptyNode =
    Html.text ""


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ToggleClientType ] [ text "Toggle Client Type" ]
        , div []
            [ text
                (if model.clientType == Presenter then
                    "Presenter Mode"

                 else
                    "Participant Mode"
                )
            ]
        , if model.clientType == Presenter then
            viewPresenter model

          else
            viewParticipant model
        ]


viewPresenter : Model -> Html Msg
viewPresenter model =
    div [ class "presenter" ]
        [ if not model.presentationStarted then
            button [ onClick StartPresentation ] [ text "Start" ]

          else
            button [ onClick StopPresentation ] [ text "Stop" ]
        , if model.presentationStarted && model.transmissionMode == VNC && model.isMobile == False then
            div []
                [ button [ onClick SelectScreen ] [ text "Select Screen" ]
                , button [ onClick SelectWindow ] [ text "Select Window" ]
                ]

          else
            emptyNode
        , if model.showPopup then
            viewPopup model

          else
            emptyNode
        ]


viewParticipant : Model -> Html Msg
viewParticipant model =
    div [ class "participant" ]
        [ h1 [] [ text model.title ]
        , div [] [ viewCanvas model ]
        , button [ onClick SelectScreen ] [ text "Select Screen" ]
        , button [ onClick SelectWindow ] [ text "Select Window" ]
        ]


viewCanvas : Model -> Html msg
viewCanvas model =
    div []
        [ if model.showCanvas then
            canvas [] []

          else
            div [] [ text "Presentation not started" ]
        ]


viewPopup : Model -> Html msg
viewPopup model =
    div []
        [ text "Popup"
        , ul []
            [ li []
                [ text "Screens"
                , if model.selection == Screen then
                    text "(selected)"

                  else
                    emptyNode
                , button [{- onClick SelectedScreen -}] [ text "Screen 1" ]
                , button [{- onClick SelectedScreen -}] [ text "Screen 2" ]
                ]
            , li []
                [ text "Windows"
                , if model.selection == Window then
                    text "(selected)"

                  else
                    emptyNode
                , button [] [ text "Window 1" ]
                , button [] [ text "Window 2" ]
                ]
            ]
        ]
