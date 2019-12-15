module Tests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Main
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


testHalfPrecisionRounding : Test
testHalfPrecisionRounding =
    let
        testCases =
            [ ( 1.0, 1.0 )
            , ( 0.5, 0.5 )
            , ( 1.1, 1.0 )
            , ( 1.26, 1.5 )
            , ( 1.57, 1.5 )
            , ( 1.79, 2.0 )
            , ( 1.9, 2.0 )
            ]
    in
    describe "Main.roundToHalves" <|
        (testCases
            |> List.map
                (\( inp, expected ) ->
                    test
                        (String.concat
                            [ "should round "
                            , String.fromFloat inp
                            , " to "
                            , String.fromFloat expected
                            ]
                        )
                    <|
                        \_ ->
                            Expect.within (Absolute 0.0)
                                (Main.roundToHalves inp)
                                expected
                )
        )
