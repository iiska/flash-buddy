module Model exposing (..)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type ISO
    = ISO Int


type alias FocalLength =
    String


type Aperture
    = Aperture Float


type GuideNumber
    = GuideNumber Float


type FlashPowerAttenuation
    = FlashPowerAttenuation Int


type alias Flash =
    { name : String
    , guideNumbers : Dict FocalLength GuideNumber
    }


type alias Model =
    { flashes : List Flash
    , selectedFlash : Maybe String
    , selectedISO : ISO
    , selectedRow : Maybe Int
    , selectedCol : Maybe Int
    , selectedFlashPower : FlashPowerAttenuation
    }


getSelectedFlash : Model -> Maybe Flash
getSelectedFlash model =
    model.selectedFlash
        |> Maybe.andThen
            (\selected ->
                model.flashes
                    |> List.filter (\f -> f.name == selected)
                    |> List.head
            )


encode : Model -> E.Value
encode model =
    let
        unwrapISO (ISO n) =
            n

        unwrapFlashPowerAttenuation (FlashPowerAttenuation n) =
            n
    in
    E.object
        [ ( "flashes", E.list encodeFlash model.flashes )
        , ( "selectedFlash"
          , model.selectedFlash
                |> Maybe.map E.string
                |> Maybe.withDefault E.null
          )
        , ( "selectedISO", model.selectedISO |> unwrapISO |> E.int )
        , ( "selectedRow"
          , model.selectedRow
                |> Maybe.map E.int
                |> Maybe.withDefault E.null
          )
        , ( "selectedCol"
          , model.selectedCol
                |> Maybe.map E.int
                |> Maybe.withDefault E.null
          )
        , ( "selectedFlashPowerAttenuation"
          , model.selectedFlashPower
                |> unwrapFlashPowerAttenuation
                |> E.int
          )
        ]


encodeFlash : Flash -> E.Value
encodeFlash flash =
    let
        encodeGuideNumber (GuideNumber n) =
            E.float n
    in
    E.object
        [ ( "name", E.string flash.name )
        , ( "guideNumbers"
          , E.dict identity
                encodeGuideNumber
                flash.guideNumbers
          )
        ]


decode : E.Value -> Result D.Error Model
decode json =
    D.decodeValue decoder json


decoder : Decoder Model
decoder =
    D.map6 Model
        (D.field "flashes" (D.list flashDecoder))
        (D.field "selectedFlash" (D.maybe D.string))
        (D.field "selectedISO" (D.int |> D.map ISO))
        (D.field "selectedRow" (D.maybe D.int))
        (D.field "selectedCol" (D.maybe D.int))
        (D.field "selectedFlashPowerAttenuation"
            (D.int
                |> D.map FlashPowerAttenuation
            )
        )


flashDecoder : Decoder Flash
flashDecoder =
    D.map2 Flash
        (D.field "name" D.string)
        (D.field "guideNumbers"
            (D.dict (D.float |> D.map GuideNumber))
        )
