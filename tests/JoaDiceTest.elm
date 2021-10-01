module JoaDiceTest exposing (..)

import Expect exposing (Expectation)
import JoaDice exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "the JoaDice module"
        [ describe "JoaDice.applyDefense"
            [ test "shields cancel attack" <|
                \_ -> Expect.equal [] (applyDefense [ Kill, Disrupt, Push ] [ Shield, Shield, Shield ])
            , test "empty defense leaves attack untouched" <|
                \_ -> Expect.equal [ Kill ] (applyDefense [ Kill ] [])
            , test "shields in attack doesn't to anything" <|
                \_ -> Expect.equal [ Disrupt ] (applyDefense [ Kill, Disrupt, Shield ] [ Shield ])
            , test "kill is canceled first" <|
                \_ -> Expect.equal [ Push, Disrupt ] (applyDefense [ Kill, Push, Disrupt ] [ Shield ])
            , test "disrupt is canceled after kill" <|
                \_ -> Expect.equal [ Push ] (applyDefense [ Kill, Push, Disrupt ] [ Shield, Shield ])
            ]
        ]
