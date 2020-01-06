module Main exposing (main, roundToHalves)

import Browser
import Browser.Navigation
import Dict
import Html exposing (Html, div, h1, header, text)
import Html.Attributes exposing (class, classList, selected, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as E
import List
import Model exposing (Aperture(..), Flash, FlashPowerAttenuation(..), GuideNumber(..), ISO(..))
import Ports
import Round


type alias Model =
    Model.Model


init : ( Model, Cmd Msg )
init =
    let
        flashes =
            [ { name = "Nissin 622 Mark 2."
              , guideNumbers =
                    Dict.fromList
                        [ ( "24", GuideNumber 25 )
                        , ( "28", GuideNumber 28 )
                        , ( "35", GuideNumber 32 )
                        , ( "50", GuideNumber 35 )
                        , ( "70", GuideNumber 38 )
                        , ( "85", GuideNumber 41 )
                        , ( "105", GuideNumber 44 )
                        ]
              }
            ]
    in
    ( { flashes = flashes
      , selectedFlash = flashes |> List.head |> Maybe.map .name
      , selectedISO = ISO 100
      , selectedRow = Nothing
      , selectedCol = Nothing
      , selectedFlashPower = FlashPowerAttenuation 1
      , localStorage = Model.NotAvailable
      , serviceWorker = Model.Latest
      }
    , Cmd.none
    )


roundToHalves : Float -> Float
roundToHalves val =
    let
        down =
            toFloat <| floor val

        half =
            toFloat (floor val) + 0.5

        up =
            toFloat (floor val) + 1.0
    in
    [ ( abs (val - down), down ), ( abs (val - half), half ), ( abs (val - up), up ) ]
        |> List.sortBy (\( diff, _ ) -> diff)
        |> List.head
        |> Maybe.map (\( _, ret ) -> ret)
        |> Maybe.withDefault down



---- UPDATE ----


type Msg
    = SelectFlash String
    | SelectISO String
    | SelectPower String
    | ToggleRow Int
    | ToggleCol Int
    | ToggleCell Int Int
    | StateRestored E.Value
    | RejectLocalStorage
    | AcceptLocalStorage
    | RejectServiceWorkerUpdate
    | AcceptServiceWorkerUpdate


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
    (case msg of
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

        SelectPower p ->
            ( { model
                | selectedFlashPower =
                    String.toInt p
                        |> Maybe.map FlashPowerAttenuation
                        |> Maybe.withDefault (FlashPowerAttenuation 1)
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

        RejectLocalStorage ->
            ( { model | localStorage = Model.NotAvailable }, Cmd.none )

        AcceptLocalStorage ->
            ( { model | localStorage = Model.InUse }, Cmd.none )

        RejectServiceWorkerUpdate ->
            ( { model | serviceWorker = Model.UpgradeLater }, Cmd.none )

        AcceptServiceWorkerUpdate ->
            ( { model | serviceWorker = Model.Latest }
            , Browser.Navigation.reloadAndSkipCache
            )

        StateRestored json ->
            ( Model.decode model json
            , Cmd.none
            )
    )
        |> cacheState


cacheState : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
cacheState ( newModel, newCmd ) =
    if newModel.localStorage == Model.InUse then
        ( newModel
        , Cmd.batch
            [ newCmd
            , newModel
                |> Model.encode
                |> Ports.storeStateCache
            ]
        )

    else
        ( newModel, newCmd )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        [ header []
            [ viewNotifications model
            , h1 [] [ text "📸 Flash Buddy" ]
            ]
        , viewFlashSelect model
        , viewFlashPowerSelect model
        , viewISOSelect model
        , viewResults model
        ]


formComponent : String -> List (Html Msg) -> Html Msg
formComponent label children =
    Html.label [] (Html.span [ class "form-label" ] [ text label ] :: children)


viewFlashSelect :
    { m | flashes : List Flash, selectedFlash : Maybe String }
    -> Html Msg
viewFlashSelect { flashes, selectedFlash } =
    formComponent "Flash"
        [ Html.select [ onInput SelectFlash ]
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
        ]


viewFlashPowerSelect :
    { m | selectedFlashPower : FlashPowerAttenuation }
    -> Html Msg
viewFlashPowerSelect { selectedFlashPower } =
    let
        powerAsInt (FlashPowerAttenuation n) =
            n

        values =
            [ ( 1, "Full" )
            , ( 2, "1/2" )
            , ( 4, "1/4" )
            , ( 8, "1/8" )
            , ( 16, "1/16" )
            , ( 32, "1/32" )
            ]
    in
    formComponent "Flash power"
        [ Html.select [ onInput SelectPower ]
            (values
                |> List.map
                    (\( val, label ) ->
                        Html.option
                            [ value <| String.fromInt val
                            , selected <| val == powerAsInt selectedFlashPower
                            ]
                            [ text label ]
                    )
            )
        ]


viewISOSelect : { m | selectedISO : ISO } -> Html Msg
viewISOSelect { selectedISO } =
    let
        values =
            [ 50, 100, 200, 400, 800, 1600, 3200, 6400, 12800 ]

        unwrap (ISO n) =
            n
    in
    formComponent "ISO"
        [ Html.select [ onInput SelectISO ]
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
        ]


apertures : List Float
apertures =
    [ 1.4, 2.0, 2.8, 4, 5.6, 8, 11, 16, 22 ]


maybeEquals : Maybe a -> a -> Bool
maybeEquals maybeA a =
    maybeA |> Maybe.map ((==) a) |> Maybe.withDefault False


distance : ISO -> Aperture -> FlashPowerAttenuation -> GuideNumber -> Float
distance (ISO iso) (Aperture aperture) (FlashPowerAttenuation fp) (GuideNumber gn) =
    let
        isoFactor =
            sqrt 2 ^ logBase 2 (toFloat iso / 100.0)

        fpFactor =
            1 / (sqrt 2 ^ logBase 2 (toFloat fp))
    in
    gn / aperture * isoFactor * fpFactor


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
                                    [ text focal ]
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
            distance iso (Aperture aperture) model.selectedFlashPower gn
                |> roundToHalves
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
                            |> List.indexedMap
                                (distanceCell index
                                    model.selectedISO
                                    aperture
                                )
                       )
                )
    in
    Model.getSelectedFlash model
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


type alias Notification =
    { title : String
    , rejectLabel : Maybe String
    , acceptLabel : Maybe String
    , onReject : Msg
    , onAccept : Msg
    }


viewNotifications : Model -> Html Msg
viewNotifications model =
    let
        maybeLocalStoreConsent =
            if model.localStorage == Model.Available then
                Just
                    (Notification "Use browser local storage to save Flash Buddy data?"
                        Nothing
                        Nothing
                        RejectLocalStorage
                        AcceptLocalStorage
                    )

            else
                Nothing

        maybeUpgradeServiceWorker =
            if model.serviceWorker == Model.UpdateAvailable then
                Just
                    (Notification "Flash Buddy upgrade available. Refresh to upgrade?"
                        (Just "Later")
                        (Just "Upgrade now!")
                        RejectServiceWorkerUpdate
                        AcceptServiceWorkerUpdate
                    )

            else
                Nothing

        notifications =
            [ maybeLocalStoreConsent, maybeUpgradeServiceWorker ]
                |> List.filterMap identity
    in
    if List.length notifications == 0 then
        text ""

    else
        Html.ul []
            (notifications
                |> List.map
                    (\n ->
                        Html.li [ class "notification--info" ]
                            [ text n.title
                            , Html.button [ onClick n.onReject ]
                                [ n.rejectLabel
                                    |> Maybe.withDefault "Reject"
                                    |> text
                                ]
                            , Html.button [ onClick n.onAccept ]
                                [ n.acceptLabel
                                    |> Maybe.withDefault "Accept"
                                    |> text
                                ]
                            ]
                    )
            )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always (Ports.restoredStateCache StateRestored)
        }
