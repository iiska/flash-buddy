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


type LocalStorageStatus
    = NotAvailable
    | Available
    | InUse


type alias Model =
    { flashes : List Flash
    , selectedFlash : Maybe String
    , selectedISO : ISO
    , selectedRow : Maybe Int
    , selectedCol : Maybe Int
    , selectedFlashPower : FlashPowerAttenuation
    , localStorage : LocalStorageStatus
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
        , ( "localStorage"
          , case model.localStorage of
                NotAvailable ->
                    E.string "not_available"

                Available ->
                    E.string "available"

                InUse ->
                    E.string "in_use"
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


decode : Model -> E.Value -> Model
decode defaults json =
    D.decodeValue (decoder defaults) json |> Result.withDefault defaults


decoder : Model -> Decoder Model
decoder defaults =
    D.map7 Model
        (D.maybe (D.field "flashes" (D.list flashDecoder))
            |> D.map (Maybe.withDefault defaults.flashes)
        )
        (D.maybe (D.field "selectedFlash" (D.maybe D.string))
            |> D.map (Maybe.withDefault defaults.selectedFlash)
        )
        (D.maybe
            (D.field "selectedISO" (D.int |> D.map ISO))
            |> D.map (Maybe.withDefault defaults.selectedISO)
        )
        (D.maybe
            (D.field "selectedRow" (D.maybe D.int))
            |> D.map (Maybe.withDefault defaults.selectedRow)
        )
        (D.maybe
            (D.field "selectedCol" (D.maybe D.int))
            |> D.map (Maybe.withDefault defaults.selectedCol)
        )
        (D.maybe
            (D.field "selectedFlashPowerAttenuation"
                (D.int
                    |> D.map FlashPowerAttenuation
                )
            )
            |> D.map (Maybe.withDefault defaults.selectedFlashPower)
        )
        (D.maybe
            (D.field "localStorage" localStorageStatusDecoder)
            |> D.map (Maybe.withDefault defaults.localStorage)
        )


flashDecoder : Decoder Flash
flashDecoder =
    D.map2 Flash
        (D.field "name" D.string)
        (D.field "guideNumbers"
            (D.dict (D.float |> D.map GuideNumber))
        )


localStorageStatusDecoder : Decoder LocalStorageStatus
localStorageStatusDecoder =
    D.string
        |> D.map
            (\value ->
                if value == "in_use" then
                    InUse

                else if value == "not_available" then
                    NotAvailable

                else
                    Available
            )
