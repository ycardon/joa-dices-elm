module JoaDice exposing (..)

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
    }


blackDice : Dice
blackDice =
    { name = "Black dice", faces = [ Kill, Disrupt, Disrupt, Shield, Shield, Shield ] }


redDice : Dice
redDice =
    { name = "Red dice", faces = [ Kill, Kill, Disrupt, Disrupt, Push, Shield ] }


yellowDice : Dice
yellowDice =
    { name = "Yellow dice", faces = [ Disrupt, Push, Push, Shield, Blank, Blank ] }


whiteDice : Dice
whiteDice =
    { name = "White dice", faces = [ Disrupt, Disrupt, Push, Shield, Shield, Blank ] }


giganticDice : Dice
giganticDice =
    { name = "Gigantic dice", faces = [ Kill, Disrupt, Disrupt, Push, Trample, Trample ] }


doomDice : Dice
doomDice =
    { name = "Doom dice", faces = [ Disrupt, Death, Death, Rally, Rally, DelayedRally ] }


roll : Dice -> Random.Generator Face
roll dice =
    case dice.faces of
        x :: xs ->
            Random.uniform x xs

        _ ->
            Random.constant Empty


rolln : ( Int, Dice ) -> Random.Generator Roll
rolln ( n, dice ) =
    Random.list n <| roll dice


rollDices : List ( Int, Dice ) -> Random.Generator Roll
rollDices dices =
    List.foldr (Random.map2 (++)) (Random.constant []) <| List.map rolln dices


rollDicesSet : List ( Int, Dice ) -> List ( Int, Dice ) -> Random.Generator ( Roll, Roll )
rollDicesSet set1 set2 =
    Random.pair (rollDices set1) (rollDices set2)


stringsFromRoll : Roll -> List String
stringsFromRoll =
    List.map stringFromFace


stringFromFace : Face -> String
stringFromFace face =
    case face of
        Kill ->
            "Kill"

        Disrupt ->
            "Disrupt"

        Push ->
            "Push"

        Shield ->
            "Shield"

        Blank ->
            "Blank"

        Trample ->
            "Trample"

        Death ->
            "Death"

        Rally ->
            "Rally"

        DelayedRally ->
            "DelayedRally"

        Empty ->
            "Empty"
