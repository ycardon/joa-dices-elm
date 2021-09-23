module JoaRules exposing (applyDefense)

import JoaDice exposing (..)
import List exposing (filter, foldr, length)



-- | apply defense shields on the attack and remove unrelevant faces from the attack


applyDefense : Roll -> Roll -> Roll
applyDefense attack defense =
    let
        ( result, _ ) =
            ( attack, count Shield defense ) |> cancel Kill |> cancel Disrupt |> cancel Push
    in
    result |> filter ((/=) Shield) |> filter ((/=) Blank)



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
