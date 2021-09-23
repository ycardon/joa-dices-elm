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
    , attackVsDefenseResult : Roll
    , attackDices : DiceChoice
    , defenseDices : DiceChoice
    , textInput : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { attackResult = []
      , defenseResult = []
      , attackVsDefenseResult = []
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
                , attackVsDefenseResult = applyDefense attackResult defenseResult
              }
            , Cmd.none
            )

        UserTypedText value ->
            let
                ( attackDices, defenseDices, _ ) =
                    parseDiceChoices value
            in
            ( { model
                | attackDices = attackDices
                , defenseDices = defenseDices
                , textInput = value
                , attackResult = []
                , defenseResult = []
                , attackVsDefenseResult = []
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
            ( { newModel
                | textInput = printDiceChoices ( newModel.attackDices, newModel.defenseDices )
                , attackResult = []
                , defenseResult = []
                , attackVsDefenseResult = []
              }
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
    div [ class "container" ]
        [ section [ class "section" ]
            [ h1 [ class "title is-1" ] [ text "Joa Dice" ]
            , div [ class "columns" ]
                [ div [ class "column is-one-quarter" ]
                    [ div [ class "box has-background-link-light" ]
                        [ h2 [ class "title" ] [ text "Attack" ]
                        , div [] (List.map (viewChosenDice True) model.attackDices)
                        ]
                    , viewResult model.attackResult "has-background-link"
                    ]
                , div [ class "column is-one-quarter" ]
                    [ div [ class "box has-background-primary-light" ]
                        [ h2 [ class "title" ] [ text "Defense" ]
                        , div [] (List.map (viewChosenDice False) model.defenseDices)
                        ]
                    , viewResult model.defenseResult "has-background-primary"
                    ]
                , div [ class "column" ]
                    [ div [ class "block box has-background-danger-light" ]
                        [ h2 [ class "title" ] [ text "Attack vs. Defense" ]
                        , div [ class "level" ]
                            [ input [ class "input", type_ "text", value model.textInput, onInput UserTypedText, onEnter UserPushedRollButton ] []
                            , button [ class "button is-danger mx-3", onClick UserPushedRollButton ] [ text "Roll" ]
                            , button [ class "button is-success", onClick UserPushedResetButton ] [ text "Reset" ]
                            ]
                        ]
                    , viewResult model.attackVsDefenseResult "has-background-danger"
                    ]
                ]
            ]
        ]


viewResult : Roll -> String -> Html msg
viewResult result color =
    if result /= [] then
        div [ class <| "box " ++ color ]
            [ h2 [ class "title has-text-white" ] (List.map (\f -> div [] [ text f ]) <| frequency result)
            ]

    else
        text ""


viewChosenDice : Bool -> ( Int, Dice ) -> Html Msg
viewChosenDice isAttack ( n, dice ) =
    div [ class "field" ]
        [ label [ class "label" ] [ text dice.name ]
        , div [ class "control" ]
            [ input
                [ class "input"
                , type_ "number"
                , Html.Attributes.min "0"
                , value <| String.fromInt n
                , onInput (UserUpdatedDiceChoice isAttack dice)
                , onEnter UserPushedRollButton
                ]
                []
            ]
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
