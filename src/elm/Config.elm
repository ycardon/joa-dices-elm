module Config exposing (..)

import Dict exposing (Dict)


type alias Config =
    { boolList : Dict String Bool
    , intList : Dict String Int
    }


getBool : String -> Config -> Bool
getBool key config =
    Dict.get key config.boolList
        |> Maybe.withDefault False


getInt : String -> Config -> Int
getInt key config =
    Dict.get key config.intList
        |> Maybe.withDefault -1


setBool : String -> Bool -> Config -> Config
setBool key value config =
    { config | boolList = Dict.update key (\_ -> Just value) config.boolList }


setInt : String -> Int -> Config -> Config
setInt key value config =
    { config | intList = Dict.update key (\_ -> Just value) config.intList }


init : List ( String, Bool ) -> List ( String, Int ) -> Config
init boolList intList =
    { boolList = Dict.fromList boolList
    , intList = Dict.fromList intList
    }
