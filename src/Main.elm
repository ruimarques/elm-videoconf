module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, li, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Maybe exposing (Maybe(..))


startTransmission : TransmissionMode -> Msg
startTransmission mode =
    if mode == TransmissionError then
        StopPresentation

    else
        StartPresentation


handleStart : Model -> TransmissionMode -> Msg
handleStart model mode =
    if mode == model.serverTransmissionMode then
        SetClientTransmissionMode mode

    else
        SetClientTransmissionMode TransmissionError


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


type SelectedScreen
    = Screen1
    | Screen2
    | Window1
    | Window2


type TransmissionMode
    = VNC
    | WebRTC
    | TransmissionError


type alias Model =
    { presentationStarted : Bool
    , activeTransmission : Bool
    , transmissionPaused : Bool
    , showPopup : Bool
    , screenMode : ModeSelection
    , selectedScreen : SelectedScreen
    , isMobile : Bool
    , clientTransmissionMode : TransmissionMode
    , serverTransmissionMode : TransmissionMode
    }


init : Model
init =
    { presentationStarted = False
    , activeTransmission = False
    , transmissionPaused = False
    , showPopup = False
    , screenMode = Screen
    , selectedScreen = Screen1
    , isMobile = False
    , clientTransmissionMode = VNC
    , serverTransmissionMode = WebRTC
    }



-- UPDATE


type Msg
    = StartPresentation
    | StopPresentation
    | SelectScreenMode ModeSelection
    | SelectScreen SelectedScreen
    | PauseTransmission
    | ToggleMobile
    | SetClientTransmissionMode TransmissionMode
    | SetServerTransmissionMode TransmissionMode


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartPresentation ->
            { model | presentationStarted = True }

        StopPresentation ->
            { model | presentationStarted = False, showPopup = False, activeTransmission = False }

        SelectScreenMode mode ->
            { model | screenMode = mode, showPopup = True }

        SelectScreen screen ->
            { model | selectedScreen = screen, activeTransmission = True }

        PauseTransmission ->
            { model
                | transmissionPaused =
                    if model.transmissionPaused == True then
                        False

                    else
                        True
            }

        ToggleMobile ->
            { model
                | isMobile =
                    if model.isMobile then
                        False

                    else
                        True
            }

        SetClientTransmissionMode mode ->
            { model
                | clientTransmissionMode = mode
                , presentationStarted =
                    if mode == TransmissionError then
                        False

                    else
                        True
                , activeTransmission =
                    if mode == TransmissionError then
                        False

                    else if mode == VNC && model.isMobile == True then
                        True

                    else
                        model.activeTransmission
            }

        SetServerTransmissionMode mode ->
            { model | serverTransmissionMode = mode }



-- VIEW


emptyNode : Html Msg
emptyNode =
    Html.text ""


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Server Configs" ]
        , viewServer model
        , h1 [] [ text "Presenter" ]
        , viewPresenter model
        , h1 [] [ text "Participant" ]
        , viewParticipant model
        ]


viewPresenter : Model -> Html Msg
viewPresenter model =
    div
        [ style "border" "1px solid black"
        , style "padding" "10px"
        ]
        [ if not model.presentationStarted then
            div []
                [ button [ onClick (handleStart model VNC) ] [ text "Start VNC" ]
                , button [ onClick (handleStart model WebRTC) ] [ text "Start WebRTC" ]
                ]

          else
            button [ onClick StopPresentation ] [ text "Stop" ]
        , if model.clientTransmissionMode == TransmissionError then
            div [ style "color" "red" ] [ text "Error while starting transmission" ]

          else
            emptyNode
        , if model.presentationStarted && model.clientTransmissionMode == VNC && model.isMobile == False then
            div []
                [ button [ onClick (SelectScreenMode Screen) ] [ text "Select Screen" ]
                , button [ onClick (SelectScreenMode Window) ] [ text "Select Window" ]
                ]

          else
            emptyNode
        , if model.presentationStarted && model.clientTransmissionMode == WebRTC then
            div [] [ button [ onClick (SelectScreenMode Screen) ] [ text "Select Screen" ] ]

          else
            emptyNode
        , if model.showPopup then
            viewPopup model

          else
            emptyNode
        ]


viewParticipant : Model -> Html Msg
viewParticipant model =
    div
        [ style "border" "1px solid black"
        , style "padding" "10px"
        ]
        [ h2 []
            [ text
                (if model.clientTransmissionMode == VNC && model.isMobile == True then
                    "Mobile Display"

                 else
                    case model.selectedScreen of
                        Screen1 ->
                            "Screen 1"

                        Screen2 ->
                            "Screen 2"

                        Window1 ->
                            "Window 1"

                        Window2 ->
                            "Window 2"
                )
            ]
        , div [] [ viewCanvas model ]
        ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    div
        [ style "border" "1px solid black"
        , style "padding" "10px"
        ]
        [ if model.activeTransmission then
            div []
                [ button [ onClick PauseTransmission ] [ text "Pause" ]
                , div
                    [ style "border" "1px solid red"
                    , style "padding" "10px"
                    , style "text-align" "center"
                    ]
                    [ text
                        ("Canvas"
                            ++ (if model.transmissionPaused then
                                    " (PAUSED)"

                                else
                                    ""
                               )
                        )
                    ]
                ]

          else
            div [] [ text "Presentation not started" ]
        ]


viewPopup : Model -> Html Msg
viewPopup model =
    div []
        [ text "Popup"
        , ul []
            [ li []
                [ text "Screens"
                , if model.screenMode == Screen then
                    text " (selected)"

                  else
                    emptyNode
                , button [ onClick (SelectScreen Screen1) ] [ text "Screen 1" ]
                , button [ onClick (SelectScreen Screen2) ] [ text "Screen 2" ]
                ]
            , if model.clientTransmissionMode == VNC then
                li []
                    [ text "Windows"
                    , if model.screenMode == Window then
                        text " (selected)"

                      else
                        emptyNode
                    , button [ onClick (SelectScreen Window1) ] [ text "Window 1" ]
                    , button [ onClick (SelectScreen Window2) ] [ text "Window 2" ]
                    ]

              else
                emptyNode
            ]
        ]


viewServer : Model -> Html Msg
viewServer model =
    div []
        [ button [ onClick ToggleMobile ] [ text "Toggle Mobile" ]
        , div []
            [ text
                ("Is mobile: "
                    ++ (if model.isMobile then
                            "True"

                        else
                            "False"
                       )
                )
            ]
        , div []
            [ button [ onClick (SetServerTransmissionMode VNC) ] [ text "Transmission VNC" ]
            , button [ onClick (SetServerTransmissionMode WebRTC) ] [ text "Transmission WebRTC" ]
            , button [ onClick (SetServerTransmissionMode TransmissionError) ] [ text "Transmission Error" ]
            , div []
                [ text
                    ("Transmission: "
                        ++ (case model.serverTransmissionMode of
                                VNC ->
                                    "VNC"

                                WebRTC ->
                                    "WebRTC"

                                TransmissionError ->
                                    "TransmissionError"
                           )
                    )
                ]
            ]
        ]
