module JoaDice exposing
    ( Dice
    , DiceChoice
    , Face(..)
    , Roll
    , applyDefense
    , blackDice
    , doomDice
    , giganticDice
    , redDice
    , rollDicesSet
    , whiteDice
    , yellowDice
    )

import List exposing (filter, foldr, length)
import Random


type Face
    = Kill
    | Disrupt
    | Push
    | Shield
    | Blank
    | Trample
    | Death
    | Rally
    | DelayedRally
    | Empty


type alias Roll =
    List Face


type alias Dice =
    { name : String
    , faces : List Face
    , color : String
    }


type alias DiceChoice =
    List ( Int, Dice )


blackDice : Dice
blackDice =
    { name = "Black dice", faces = [ Kill, Disrupt, Disrupt, Shield, Shield, Shield ], color = "Black" }


redDice : Dice
redDice =
    { name = "Red dice", faces = [ Kill, Kill, Disrupt, Disrupt, Push, Shield ], color = "Red" }


yellowDice : Dice
yellowDice =
    { name = "Yellow dice", faces = [ Disrupt, Push, Push, Shield, Blank, Blank ], color = "Gold" }


whiteDice : Dice
whiteDice =
    { name = "White dice", faces = [ Disrupt, Disrupt, Push, Shield, Shield, Blank ], color = "Silver" }


giganticDice : Dice
giganticDice =
    { name = "Gigantic dice", faces = [ Kill, Disrupt, Disrupt, Push, Trample, Trample ], color = "Purple" }


doomDice : Dice
doomDice =
    { name = "Doom dice", faces = [ Disrupt, Death, Death, Rally, Rally, DelayedRally ], color = "Wheat" }



-- | dice rolls


roll1 : Dice -> Random.Generator Face
roll1 dice =
    case dice.faces of
        x :: xs ->
            Random.uniform x xs

        _ ->
            Random.constant Empty


rollN : ( Int, Dice ) -> Random.Generator Roll
rollN ( n, dice ) =
    Random.list n (roll1 dice)


rollDices : DiceChoice -> Random.Generator Roll
rollDices dices =
    List.foldr (Random.map2 (++)) (Random.constant []) (List.map rollN dices)


rollDicesSet : DiceChoice -> DiceChoice -> Random.Generator ( Roll, Roll )
rollDicesSet set1 set2 =
    Random.pair (rollDices set1) (rollDices set2)



-- | apply defense shields on the attack and remove irrelevant faces from the attack


applyDefense : Roll -> Roll -> Roll
applyDefense attack defense =
    let
        ( result, _ ) =
            ( attack, count Shield defense )
                |> cancel Kill
                |> cancel Disrupt
                |> cancel Push
    in
    result
        |> filter ((/=) Shield)
        |> filter ((/=) Blank)



-- | cancel roll faces by an amount of shield count


cancel : Face -> ( Roll, Int ) -> ( Roll, Int )
cancel face ( roll, shieldCount ) =
    let
        f x ( xs, n ) =
            if x == face && n > 0 then
                ( xs, n - 1 )

            else
                ( x :: xs, n )
    in
    foldr f ( [], shieldCount ) roll



-- | count the number of a given face in a roll


count : Face -> Roll -> Int
count face roll =
    filter ((==) face) roll |> length
