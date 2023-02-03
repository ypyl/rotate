module Main exposing (main)

import Browser
import Element exposing (Element, el, text, row, alignRight, fill, width, rgb255, spacing, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


main : Program String Model Msg
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
        (text model.message)


type alias Model =
    { message : String
    }


init : String -> ( Model, Cmd Msg )
init initialMesssage =
    ( { message = initialMesssage }, Cmd.none )


type Msg
    = Name String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | message = name }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
