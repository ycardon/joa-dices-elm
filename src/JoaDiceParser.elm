module JoaDiceParser exposing (parse)

import JoaDice exposing (Dice, blackDice, doomDice, giganticDice, redDice, whiteDice, yellowDice)
import List exposing (foldl)
import String exposing (contains, dropRight, right, split, toInt, toUpper)



-- | parse a fight in the form : 3R Y - 2B 1W


parse : String -> ( List ( Int, Dice ), List ( Int, Dice ), Bool )
parse string =
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
    case str |> dropRight 1 |> toInt of
        Just n ->
            n

        Nothing ->
            1



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
