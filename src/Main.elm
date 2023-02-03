module Main exposing (main)

import Browser
import Element exposing (Element, el, text, row, alignRight, fill, width, rgb255, spacing, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Browser.Events exposing (onResize)


main : Program (Int, Int) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = myRowOfStuff >> Element.layout []
        , subscriptions = subscriptions
        }

myRowOfStuff : Model -> Element Msg
myRowOfStuff model =
    row [ width fill, centerY, spacing 30 ]
        [ myElement model
        , myElement model
        , el [ alignRight ] (myElement model)
        ]


myElement : Model -> Element msg
myElement model =
    el
        [ Background.color (rgb255 240 0 245)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 30
        ]
        (text (String.fromInt(model.windowWidth) ++ "x" ++ String.fromInt(model.windowHeight)))


type alias Model =
    { message : String
    , windowWidth : Int
    , windowHeight : Int
    }


init : (Int, Int) -> ( Model, Cmd Msg )
init (windowWidth, windowHeight) =
    ( { message = "hey", windowWidth = windowWidth, windowHeight = windowHeight }, Cmd.none )


type Msg
    = Name String
    | SetWindowWidthHeight Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | message = name }, Cmd.none )
        SetWindowWidthHeight width height ->
            ( { model | windowWidth = width, windowHeight = height }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\w h -> SetWindowWidthHeight w h)
