module JoaDiceParser exposing (parseDiceChoices, printDice, printDiceChoices)

import JoaDice exposing (..)
import List exposing (foldl)
import String exposing (contains, dropRight, right, split, toInt, toUpper)



-- | parse a fight in the form : 3R Y - 2B 1W


parseDiceChoices : String -> ( DiceChoice, DiceChoice, Bool )
parseDiceChoices string =
    let
        f str ( att, def, isDef ) =
            if contains "-" str then
                ( att, def, True )

            else
                case str |> right 1 |> toUpper |> parseDice of
                    Nothing ->
                        ( att, def, isDef )

                    Just d ->
                        if not isDef then
                            ( ( parseInt str, d ) :: att, def, isDef )

                        else
                            ( att, ( parseInt str, d ) :: def, isDef )
    in
    string |> split " " |> foldl f ( [], [], False )



-- | remove the last character and return an int or 1


parseInt : String -> Int
parseInt str =
    str |> dropRight 1 |> toInt |> Maybe.withDefault 1



-- | return the corresponding dice


parseDice : String -> Maybe Dice
parseDice char =
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


printDice : Dice -> String
printDice dice =
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


printDiceChoices : ( DiceChoice, DiceChoice ) -> String
printDiceChoices ( attackDices, defenseDices ) =
    let
        stringOrNoting n d =
            if n /= 0 then
                String.fromInt n ++ printDice d ++ " "

            else
                ""

        toString =
            List.foldl (\( n, d ) s -> s ++ stringOrNoting n d) ""

        defenseOrNothing d =
            if d /= "" then
                "- " ++ d

            else
                ""
    in
    toString attackDices ++ defenseOrNothing (toString defenseDices)
