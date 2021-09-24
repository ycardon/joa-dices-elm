module JoaDiceParser exposing
    ( decodeDiceChoices
    , encodeChoices
    , encodeDice
    )

import JoaDice exposing (..)
import List exposing (foldl)
import String exposing (contains, dropRight, right, split, toInt, toUpper)



-- DECODERS (format is in the form of:  3R Y - 2B 1W


decodeDiceChoices : String -> ( DiceChoice, DiceChoice, Bool )
decodeDiceChoices string =
    let
        f str ( att, def, isDef ) =
            if contains "-" str then
                ( att, def, True )

            else
                case str |> right 1 |> toUpper |> decodeDice of
                    Nothing ->
                        ( att, def, isDef )

                    Just d ->
                        if not isDef then
                            ( ( decodeInt str, d ) :: att, def, isDef )

                        else
                            ( att, ( decodeInt str, d ) :: def, isDef )
    in
    string |> split " " |> foldl f ( [], [], False )


decodeInt : String -> Int
decodeInt str =
    str |> dropRight 1 |> toInt |> Maybe.withDefault 1


decodeDice : String -> Maybe Dice
decodeDice char =
    case char of
        "B" ->
            Just blackDice

        "R" ->
            Just redDice

        "Y" ->
            Just yellowDice

        "W" ->
            Just whiteDice

        "G" ->
            Just giganticDice

        "D" ->
            Just doomDice

        _ ->
            Nothing



-- ENCODERS


encodeChoices : ( DiceChoice, DiceChoice ) -> String
encodeChoices ( attackDices, defenseDices ) =
    let
        stringOrNoting n d =
            if n <= 0 then
                ""

            else if n == 1 then
                encodeDice d ++ " "

            else
                String.fromInt n ++ encodeDice d ++ " "

        toString =
            List.foldl (\( n, d ) s -> s ++ stringOrNoting n d) ""

        defenseOrNothing d =
            if d /= "" then
                "- " ++ d

            else
                ""
    in
    toString attackDices ++ defenseOrNothing (toString defenseDices)


encodeDice : Dice -> String
encodeDice dice =
    if dice == blackDice then
        "B"

    else if dice == redDice then
        "R"

    else if dice == yellowDice then
        "Y"

    else if dice == yellowDice then
        "Y"

    else if dice == whiteDice then
        "W"

    else if dice == giganticDice then
        "G"

    else if dice == doomDice then
        "D"

    else
        ""



-- | pretty print a face
