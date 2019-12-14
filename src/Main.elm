module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (classList, selected, src, value)
import Html.Events exposing (onClick, onInput)
import List
import Round



---- MODEL ----
-- Distance * Aperture = Guide Number
--


type ISO
    = ISO Int


type alias FocalLength =
    Int


type Aperture
    = Aperture Float


type GuideNumber
    = GuideNumber Float


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
    }


init : ( Model, Cmd Msg )
init =
    let
        flashes =
            [ { name = "Nissin 622 Mark 2."
              , guideNumbers =
                    Dict.fromList
                        [ ( 24, GuideNumber 25 )
                        , ( 28, GuideNumber 28 )
                        , ( 35, GuideNumber 32 )
                        , ( 50, GuideNumber 35 )
                        , ( 70, GuideNumber 38 )
                        , ( 85, GuideNumber 41 )
                        , ( 105, GuideNumber 44 )
                        ]
              }
            ]
    in
    ( { flashes = flashes
      , selectedFlash = flashes |> List.head |> Maybe.map .name
      , selectedISO = ISO 100
      , selectedRow = Nothing
      , selectedCol = Nothing
      }
    , Cmd.none
    )


getSelectedFlash : Model -> Maybe Flash
getSelectedFlash model =
    model.selectedFlash
        |> Maybe.andThen
            (\selected ->
                model.flashes
                    |> List.filter (\f -> f.name == selected)
                    |> List.head
            )



---- UPDATE ----


type Msg
    = SelectFlash String
    | SelectISO String
    | ToggleRow Int
    | ToggleCol Int
    | ToggleCell Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toggle maybeInt newInt =
            case maybeInt of
                Just n ->
                    if n == newInt then
                        Nothing

                    else
                        Just newInt

                Nothing ->
                    Just newInt
    in
    case msg of
        SelectFlash name ->
            ( { model | selectedFlash = Just name }, Cmd.none )

        SelectISO val ->
            ( { model
                | selectedISO =
                    String.toInt val
                        |> Maybe.map ISO
                        |> Maybe.withDefault (ISO 100)
              }
            , Cmd.none
            )

        ToggleRow n ->
            ( { model
                | selectedRow = toggle model.selectedRow n
              }
            , Cmd.none
            )

        ToggleCol n ->
            ( { model | selectedCol = toggle model.selectedCol n }, Cmd.none )

        ToggleCell row col ->
            ( { model
                | selectedRow = toggle model.selectedRow row
                , selectedCol = toggle model.selectedCol col
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Flash buddy" ]
        , viewFlashSelect model
        , viewISOSelect model
        , viewResults model
        ]


viewFlashSelect :
    { m | flashes : List Flash, selectedFlash : Maybe String }
    -> Html Msg
viewFlashSelect { flashes, selectedFlash } =
    Html.select [ onInput SelectFlash ]
        (flashes
            |> List.map
                (\flash ->
                    Html.option
                        [ value flash.name
                        , selected <|
                            (selectedFlash
                                |> Maybe.map ((==) flash.name)
                                |> Maybe.withDefault False
                            )
                        ]
                        [ text flash.name ]
                )
        )


viewISOSelect : { m | selectedISO : ISO } -> Html Msg
viewISOSelect { selectedISO } =
    let
        values =
            [ 50, 100, 200, 400, 800, 1600, 3200, 6400, 12800 ]

        unwrap (ISO n) =
            n
    in
    Html.select [ onInput SelectISO ]
        (values
            |> List.map
                (\iso ->
                    Html.option
                        [ value <| String.fromInt iso
                        , selected <| iso == unwrap selectedISO
                        ]
                        [ text <| String.fromInt iso ]
                )
        )


apertures =
    [ 1.4, 2.0, 2.8, 4, 5.6, 8, 11, 16, 22 ]


guideNumber : GuideNumber -> Float
guideNumber (GuideNumber gn) =
    gn


maybeEquals : Maybe a -> a -> Bool
maybeEquals maybeA a =
    maybeA |> Maybe.map ((==) a) |> Maybe.withDefault False


viewResults : Model -> Html Msg
viewResults model =
    let
        focalLenghtHeader flash =
            Html.th [] [ text "" ]
                :: (Dict.keys flash.guideNumbers
                        |> List.indexedMap
                            (\index focal ->
                                Html.th
                                    [ onClick (ToggleCol index)
                                    , classList
                                        [ ( "col-selected"
                                          , maybeEquals model.selectedCol index
                                          )
                                        ]
                                    ]
                                    [ text <| String.fromInt focal ]
                            )
                   )

        apertureLabel index aperture =
            Html.th
                [ onClick (ToggleRow index)
                , classList
                    [ ( "row-selected"
                      , maybeEquals model.selectedRow index
                      )
                    ]
                ]
                [ text <| String.fromFloat aperture ]

        distanceCell row iso aperture col gn =
            gn
                |> guideNumber
                |> (\n -> n / aperture)
                |> Round.round 1
                |> text
                |> List.singleton
                |> Html.td
                    [ onClick (ToggleCell row col)
                    , classList
                        [ ( "row-selected", maybeEquals model.selectedRow row )
                        , ( "col-selected", maybeEquals model.selectedCol col )
                        ]
                    ]

        apertureRow flash index aperture =
            Html.tr []
                (apertureLabel index aperture
                    :: (Dict.values flash.guideNumbers
                            |> List.indexedMap (distanceCell index aperture)
                       )
                )
    in
    getSelectedFlash model
        |> Maybe.map
            (\flash ->
                Html.table []
                    (Html.tr [] (focalLenghtHeader flash)
                        :: (apertures
                                |> List.indexedMap (apertureRow flash)
                           )
                    )
            )
        |> Maybe.withDefault (text "")



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
