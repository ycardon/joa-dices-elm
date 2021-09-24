module Main exposing (main, printRoll)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import JoaDice exposing (..)
import JoaDiceParser exposing (decodeDiceChoices, encodeChoices)
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
                    decodeDiceChoices value
            in
            ( resetResult
                { model
                    | attackDices = attackDices
                    , defenseDices = defenseDices
                    , textInput = value
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
            ( resetResult
                { newModel | textInput = encodeChoices ( newModel.attackDices, newModel.defenseDices ) }
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


resetResult : Model -> Model
resetResult model =
    { model
        | attackResult = []
        , defenseResult = []
        , attackVsDefenseResult = []
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ viewHeader ()
                , div [ class "columns" ]
                    [ div [ class "column is-two-fifths" ]
                        [ div [ class "block box has-background-danger-light" ]
                            [ h2 [ class "title is-hidden-mobile" ] [ text "Attack vs. Defense" ]
                            , div [ class "level" ]
                                [ input
                                    [ class "input"
                                    , type_ "text"
                                    , value model.textInput
                                    , placeholder "2B R - 3W"
                                    , onInput UserTypedText
                                    , onEnter UserPushedRollButton
                                    ]
                                    []
                                , button
                                    [ class "button is-danger mx-3 is-hidden-mobile"
                                    , onClick UserPushedRollButton
                                    ]
                                    [ text "Roll" ]
                                , button
                                    [ class "button is-success is-hidden-mobile"
                                    , onClick UserPushedResetButton
                                    ]
                                    [ text "Reset" ]
                                ]
                            ]
                        , if model.attackResult /= [] then
                            viewResult model.attackVsDefenseResult True "has-background-danger"

                          else
                            text ""
                        ]
                    , div [ class "column" ]
                        [ div [ class "box has-background-link-light is-hidden-mobile" ]
                            [ h2 [ class "title" ] [ text "Attack" ]
                            , div [] (List.map (viewChosenDiceSelector True) model.attackDices)
                            ]
                        , viewResult model.attackResult False "has-background-link"
                        ]
                    , div [ class "column" ]
                        [ div [ class "box has-background-primary-light is-hidden-mobile" ]
                            [ h2 [ class "title" ] [ text "Defense" ]
                            , div [] (List.map (viewChosenDiceSelector False) model.defenseDices)
                            ]
                        , viewResult model.defenseResult False "has-background-primary"
                        ]
                    ]
                ]
            ]
        , viewFooter ()
        ]


viewResult : Roll -> Bool -> String -> Html msg
viewResult result isAttackVsDefense color =
    let
        filledResult =
            if result == [] && isAttackVsDefense then
                [ text "Attack failed" ]

            else
                List.map (\f -> div [] [ text f ]) <| printRoll result
    in
    if filledResult /= [] then
        div [ class <| "box " ++ color ]
            [ h2 [ class "title has-text-white" ] filledResult
            ]

    else
        text ""


viewChosenDiceSelector : Bool -> ( Int, Dice ) -> Html Msg
viewChosenDiceSelector isAttack ( n, dice ) =
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


viewHeader : () -> Html msg
viewHeader () =
    div [ class "block" ]
        [ h1 [ class "title is-1" ] [ text "JoA Dice" ]
        , h1 [ class "subtitle is-hidden-mobile" ]
            [ text "a helper for "
            , a [ href "https://mythicgames.net/board-games/tol-joan-of-arc/" ] [ text "Time of Legends: Joan of Arc" ]
            ]
        ]


viewFooter : () -> Html msg
viewFooter () =
    footer [ class "footer is-hidden-mobile" ]
        [ div [ class "content has-text-centered" ]
            [ text <| "made with " ++ heartString ++ ", "
            , a [ href "https://elm-lang.org" ] [ text "elm" ]
            , text " and "
            , a [ href "https://bulma.io" ] [ text "bulma" ]
            ]
        ]



-- SUPPORT FUNCTIONS


intFromString : String -> Int
intFromString value =
    Maybe.withDefault 0 (String.toInt value)


heartString =
    String.fromChar (Char.fromCode 10084)


printRoll : Roll -> List String
printRoll roll =
    roll
        |> List.map printFace
        |> List.sort
        |> List.Extra.group
        |> List.map
            (\( x, xs ) -> String.fromInt (List.length xs + 1) ++ " " ++ x)


printFace : Face -> String
printFace face =
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
