module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import JoaDice exposing (..)
import JoaDiceParser exposing (parseDiceChoices, printDiceChoices)
import JoaRules exposing (applyDefense)
import List.Extra
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { attackResult : Roll
    , defenseResult : Roll
    , total : Roll
    , attackDices : DiceChoice
    , defenseDices : DiceChoice
    , textInput : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { attackResult = []
      , defenseResult = []
      , total = []
      , attackDices =
            [ ( 0, blackDice )
            , ( 0, redDice )
            , ( 0, yellowDice )
            , ( 0, whiteDice )
            , ( 0, giganticDice )
            , ( 0, doomDice )
            ]
      , defenseDices =
            [ ( 0, blackDice )
            , ( 0, redDice )
            , ( 0, yellowDice )
            , ( 0, whiteDice )
            , ( 0, giganticDice )
            , ( 0, doomDice )
            ]
      , textInput = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UserTypedText String
    | UserPushedRollButton
    | UserPushedResetButton
    | UserUpdatedDiceChoice Bool Dice String
    | NewRollResult ( Roll, Roll )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserTypedText value ->
            let
                ( attackDices, defenseDices, _ ) =
                    parseDiceChoices value
            in
            ( { model
                | attackDices = attackDices
                , defenseDices = defenseDices
                , textInput = value
              }
            , Cmd.none
            )

        UserPushedRollButton ->
            ( model
            , Random.generate NewRollResult (rollDicesSet model.attackDices model.defenseDices)
            )

        UserPushedResetButton ->
            init ()

        NewRollResult ( attackResult, defenseResult ) ->
            ( { model
                | attackResult = attackResult
                , defenseResult = defenseResult
                , total = applyDefense attackResult defenseResult
              }
            , Cmd.none
            )

        UserUpdatedDiceChoice isAttack dice value ->
            let
                newModel =
                    if isAttack then
                        { model | attackDices = updateDiceChoice ( intFromString value, dice ) model.attackDices }

                    else
                        { model | defenseDices = updateDiceChoice ( intFromString value, dice ) model.defenseDices }
            in
            ( { newModel | textInput = printDiceChoices ( newModel.attackDices, newModel.defenseDices ) }
            , Cmd.none
            )


updateDiceChoice : ( Int, Dice ) -> DiceChoice -> DiceChoice
updateDiceChoice ( value, dice ) diceChoice =
    let
        f ( n, d ) =
            if d == dice then
                ( value, d )

            else
                ( n, d )
    in
    List.map f diceChoice



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Attack" ]
        , div [] (List.map (viewChosenDice True) model.attackDices)
        , viewResult model.attackResult
        , h2 [] [ text "Defense" ]
        , div [] (List.map (viewChosenDice False) model.defenseDices)
        , viewResult model.defenseResult
        , input [ type_ "text", value model.textInput, onInput UserTypedText, onEnter UserPushedRollButton ] []
        , button [ onClick UserPushedRollButton ] [ text "Roll" ]
        , button [ onClick UserPushedResetButton ] [ text "Reset" ]
        , h2 [] [ text "Attack vs. Defense" ]
        , viewResult model.total
        ]


viewResult : Roll -> Html msg
viewResult result =
    h4 [] (List.map (\f -> div [] [ text f ]) <| frequency result)


viewChosenDice : Bool -> ( Int, Dice ) -> Html Msg
viewChosenDice isAttack ( n, dice ) =
    div []
        [ input
            [ type_ "number"
            , Html.Attributes.min "0"
            , value <| String.fromInt n
            , onInput (UserUpdatedDiceChoice isAttack dice)
            ]
            []
        , text dice.name
        ]



-- SUPPORT FUNCTIONS


intFromString : String -> Int
intFromString value =
    Maybe.withDefault 0 (String.toInt value)


frequency : Roll -> List String
frequency roll =
    roll
        |> List.map stringFromFace
        |> List.sort
        |> List.Extra.group
        |> List.map
            (\( x, xs ) -> String.fromInt (List.length xs + 1) ++ " " ++ x)
