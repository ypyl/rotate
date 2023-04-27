module TimePicker exposing (..)

import Colors exposing (blue, gray)
import Element exposing (Element, alignRight, column, el, fill, height, html, none, paddingXY, px, row, spacing, text, transparent, width)
import Element.Background
import Element.Border
import Element.Events exposing (onClick)
import Element.Font exposing (center)
import Element.Input exposing (button)
import Html.Attributes exposing (style)
import TimeOnly as TO
import TypedSvg exposing (path, svg)
import TypedSvg.Attributes as TSA
import TypedSvg.Types exposing (Length(..), Paint(..), StrokeLinecap(..), StrokeLinejoin(..), YesNo(..))



-- type Model
--     = Model InternalModel
-- create : Maybe TO.TimeOnly -> Model
-- create timeOnly =
--     Model { timeOnly = timeOnly }
-- getTimeOnly : Model -> Maybe TO.TimeOnly
-- getTimeOnly model =
--     extractModel model |> .timeOnly
-- type alias InternalModel =
--     { timeOnly : Maybe TO.TimeOnly
--     }


type Msg
    = NoOp
    | Switch Bool
    | AddHour
    | RemoveHour
    | ChangeAMPM
    | Add15Mins
    | Remove15Mins


update : Msg -> Maybe TO.TimeOnly -> Maybe TO.TimeOnly
update msg model =
    case msg of
        NoOp ->
            model

        Switch True ->
            Just (TO.add (TO.hours 12) (TO.minutes 0))

        Switch False ->
            Nothing

        AddHour ->
            case model of
                Just t ->
                    Just (TO.add t (TO.hours 1))

                Nothing ->
                    model

        RemoveHour ->
            case model of
                Just t ->
                    Just (TO.minus t (TO.hours 1))

                Nothing ->
                    model

        ChangeAMPM ->
            case model of
                Just t ->
                    Just (TO.add t (TO.hours 12))

                Nothing ->
                    model

        Add15Mins ->
            case model of
                Just t ->
                    Just (TO.add t (TO.minutes 15))

                Nothing ->
                    model

        Remove15Mins ->
            case model of
                Just t ->
                    Just (TO.minus t (TO.minutes 15))

                Nothing ->
                    model


view : Maybe TO.TimeOnly -> Element Msg
view model =
    case model of
        Just timeValue ->
            row [ height fill, spacing 5 ]
                [ row [ spacing 5, paddingXY 5 0 ]
                    [ column []
                        [ button [ width fill, center ] { onPress = Just ChangeAMPM, label = upArrow }
                        , getAMorPM timeValue |> toString |> text |> el []
                        , button [ width fill, center ] { onPress = Just ChangeAMPM, label = downArrow }
                        ]
                    , column []
                        [ button [ width fill, center ] { onPress = Just AddHour, label = upArrow }
                        , TO.getHours timeValue |> hoursUsingAMPM |> String.fromInt |> String.padLeft 2 '0' |> text |> el []
                        , button [ width fill, center ] { onPress = Just RemoveHour, label = downArrow }
                        ]
                    , el [] (text ":")
                    , column []
                        [ button [ width fill, center ] { onPress = Just Add15Mins, label = upArrow }
                        , TO.getMinutes timeValue |> String.fromInt |> String.padLeft 2 '0' |> text |> el []
                        , button [ width fill, center ] { onPress = Just Remove15Mins, label = downArrow }
                        ]
                    ]
                , el [ height fill, width (px 15), Element.Background.color blue, Element.Border.rounded 3, alignRight, onClick (Switch False) ] none
                ]
                |> el [ height fill, Element.Border.rounded 3 ]

        Nothing ->
            row [ height fill ]
                [ el [ height fill, width (px 15), Element.Background.color gray, Element.Border.rounded 3, onClick (Switch True) ] none
                , el [ spacing 5, paddingXY 5 0, transparent True, width (px 110), height (px 52) ] none
                ]
                |> el [ height fill, Element.Border.rounded 3 ]


hoursUsingAMPM : Int -> Int
hoursUsingAMPM hours =
    if hours == 0 then
        12

    else if hours > 12 then
        hours - 12

    else
        hours


upArrow : Element Msg
upArrow =
    svg
        [ style "vertical-align" "middle"
        , TSA.width <| Px 16
        , TSA.height <| Px 16
        , TSA.viewBox 0 0 24 24
        , TSA.strokeWidth <| Px 1
        , TSA.fill PaintNone
        , TSA.strokeLinecap StrokeLinecapRound
        , TSA.strokeLinejoin StrokeLinejoinRound
        ]
        [ path [ TSA.stroke PaintNone, TSA.d "M0 0h24v24H0z", TSA.fill PaintNone ] []
        , path [ TSA.d "M6 15l6 -6l6 6" ] []
        ]
        |> html


downArrow : Element Msg
downArrow =
    svg
        [ style "vertical-align" "middle"
        , TSA.width <| Px 16
        , TSA.height <| Px 16
        , TSA.viewBox 0 0 24 24
        , TSA.strokeWidth <| Px 1
        , TSA.fill PaintNone
        , TSA.strokeLinecap StrokeLinecapRound
        , TSA.strokeLinejoin StrokeLinejoinRound
        ]
        [ path [ TSA.stroke PaintNone, TSA.d "M0 0h24v24H0z", TSA.fill PaintNone ] []
        , path [ TSA.d "M6 9l6 6l6 -6" ] []
        ]
        |> html


type AMPM
    = AM
    | PM


getAMorPM : TO.TimeOnly -> AMPM
getAMorPM timeOnly =
    if TO.getHours timeOnly >= 12 then
        PM

    else
        AM


toString : AMPM -> String
toString ampm =
    case ampm of
        AM ->
            "AM"

        PM ->
            "PM"
