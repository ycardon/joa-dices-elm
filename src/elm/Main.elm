module Main exposing (main, printRoll)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import JoaDice exposing (..)
import JoaDiceParser exposing (decodeDiceChoices, encodeDiceChoices)
import List.Extra
import Random



-- CONFIG


config =
    { enableColoredLabel = True
    , enableHideGiganticAndDoomDice = True
    , enableAddMissingDiceChoice = True
    }



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type AttackState
    = AttackSucceeded
    | AttackFailed
    | NoAttack


type alias Model =
    { attackDices : DiceChoice
    , defenseDices : DiceChoice
    , textInput : String
    , attackResult : Roll
    , defenseResult : Roll
    , finalResult : Roll
    , attackState : AttackState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { attackDices = initialDiceChoice
      , defenseDices = initialDiceChoice
      , textInput = ""
      , attackResult = []
      , defenseResult = []
      , finalResult = []
      , attackState = NoAttack
      }
    , Cmd.none
    )


initialDiceChoice : DiceChoice
initialDiceChoice =
    if config.enableHideGiganticAndDoomDice then
        [ ( 0, blackDice )
        , ( 0, redDice )
        , ( 0, yellowDice )
        , ( 0, whiteDice )
        ]

    else
        [ ( 0, blackDice )
        , ( 0, redDice )
        , ( 0, yellowDice )
        , ( 0, whiteDice )
        , ( 0, giganticDice )
        , ( 0, doomDice )
        ]


addMissingDiceChoice : DiceChoice -> DiceChoice
addMissingDiceChoice diceChoice =
    if config.enableAddMissingDiceChoice then
        let
            isNotInside ( _, dice ) updated =
                List.isEmpty <| List.filter (\( _, d ) -> dice == d) updated

            calcUpdated ( value, dice ) updated =
                List.map
                    (\( n, d ) ->
                        if d == dice then
                            ( value + n, d )

                        else
                            ( n, d )
                    )
                    updated

            f : ( Int, Dice ) -> DiceChoice -> DiceChoice
            f choice updated =
                if isNotInside choice updated then
                    List.append updated [ choice ]

                else
                    calcUpdated choice updated
        in
        List.foldr f initialDiceChoice diceChoice

    else
        diceChoice



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
            let
                attackVsDefenseResult =
                    applyDefense attackResult defenseResult

                attackState =
                    if attackVsDefenseResult == [] then
                        AttackFailed

                    else
                        AttackSucceeded
            in
            ( { model
                | attackResult = attackResult
                , defenseResult = defenseResult
                , finalResult = attackVsDefenseResult
                , attackState = attackState
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
                    | attackDices = addMissingDiceChoice attackDices
                    , defenseDices = addMissingDiceChoice defenseDices
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
                { newModel | textInput = encodeDiceChoices ( newModel.attackDices, newModel.defenseDices ) }
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
        , finalResult = []
        , attackState = NoAttack
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
                    [ viewTextInputAndFinalResultSection model
                    , viewDiceSelectionAndResultSection model True
                    , viewDiceSelectionAndResultSection model False
                    ]
                ]
            ]
        , viewFooter ()
        ]


viewTextInputAndFinalResultSection : Model -> Html Msg
viewTextInputAndFinalResultSection model =
    div [ class "column is-two-fifths" ]
        [ div [ class "block box has-background-danger-light" ]
            [ h2 [ class "title is-hidden-mobile" ] [ text "Attack vs. Defense" ]
            , div [ class "field has-addons" ]
                [ p [ class "control is-expanded" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , value model.textInput
                        , placeholder "2B R - 3W"
                        , onInput UserTypedText
                        , onEnter UserPushedRollButton
                        ]
                        []
                    ]
                , p [ class "control is-hidden-mobile" ]
                    [ button [ class "button has-background-light", onClick UserPushedResetButton ]
                        [ text "Reset" ]
                    ]
                , p [ class "control" ]
                    [ button [ class "button is-rounded is-danger", onClick UserPushedRollButton ]
                        [ text "Roll" ]
                    ]
                ]
            ]
        , viewResult model.attackState model.finalResult True "has-background-danger"
        ]


viewDiceSelectionAndResultSection : Model -> Bool -> Html Msg
viewDiceSelectionAndResultSection model isAttack =
    let
        x =
            if isAttack then
                { diceChoice = model.attackDices
                , result = model.attackResult
                , name = "Attack"
                , color = "has-background-link"
                }

            else
                { diceChoice = model.defenseDices
                , result = model.defenseResult
                , name = "Defense"
                , color = "has-background-primary"
                }
    in
    div [ class "column" ]
        [ div [ class <| "box is-hidden-mobile " ++ x.color ++ "-light" ]
            [ h2 [ class "title" ] [ text x.name ]
            , div [] (List.map (viewChosenDiceSelector isAttack) x.diceChoice)
            ]
        , viewResult model.attackState x.result False x.color
        ]


viewResult : AttackState -> Roll -> Bool -> String -> Html msg
viewResult attackState result isTextInputAndFinalResultSection color =
    if attackState /= NoAttack && (result /= [] || isTextInputAndFinalResultSection) then
        div [ class <| "box " ++ color ]
            [ h2 [ class "title has-text-white" ]
                (if attackState == AttackFailed && isTextInputAndFinalResultSection then
                    [ text "Attack failed" ]

                 else
                    List.map (\f -> div [] [ text f ]) <| printRoll result
                )
            ]

    else
        text ""


viewChosenDiceSelector : Bool -> ( Int, Dice ) -> Html Msg
viewChosenDiceSelector isAttack ( n, dice ) =
    div [ class "field" ]
        [ coloredDiceLabel config.enableHideGiganticAndDoomDice ( n, dice )
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
        , p [ class "help" ] [ text <| printDice dice ]
        ]


coloredDiceLabel : Bool -> ( Int, Dice ) -> Html msg
coloredDiceLabel isEnabled ( n, dice ) =
    if isEnabled then
        label [ class "label" ]
            [ i
                [ class <| fontAwesome n
                , style "color" dice.color
                , style "text-shadow" "-2px 2px 4px Silver, 2px -2px 0 White"
                ]
                []
            , text <| " " ++ dice.name
            ]

    else
        label [ class "label" ] [ text <| dice.name ]


viewHeader : () -> Html msg
viewHeader () =
    div [ class "block" ]
        [ h1 [ class "title is-1" ] [ text "JoA Dice" ]
        , h1 [ class "subtitle is-hidden-mobile" ]
            [ text "a helper for "
            , a [ href "https://mythicgames.net/board-games/tol-joan-of-arc/" ]
                [ text "Time of Legends: Joan of Arc" ]
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
            , p []
                [ a [ href "https://github.com/topics/joa-dices-rewrite" ] [ text "joa-dices-rewrite" ]
                , text " collection 2021"
                ]
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


printDice : Dice -> String
printDice dice =
    List.foldl (\f s -> s ++ printFace f ++ " ") "" dice.faces


fontAwesome : Int -> String
fontAwesome n =
    case n of
        0 ->
            ""

        1 ->
            "fas fa-dice-one"

        2 ->
            "fas fa-dice-two"

        3 ->
            "fas fa-dice-three"

        4 ->
            "fas fa-dice-four"

        5 ->
            "fas fa-dice-five"

        6 ->
            "fas fa-dice-six"

        _ ->
            "fas fa-dice"
