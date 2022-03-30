module Main exposing (main, printRoll)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import JoaDice exposing (..)
import JoaDiceParser exposing (decodeDiceChoices, encodeDiceChoices)
import List.Extra
import Random



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { attackDices : DiceChoice
    , defenseDices : DiceChoice
    , textInput : String
    , attackResult : AttackResult
    , isConfigOpen : Bool
    , config : Config
    }


type AttackResult
    = NoAttack
    | AttackResult
        { attackResult : Roll
        , defenseResult : Roll
        , finalResult : Roll
        , isSuccess : Bool
        }


type alias Config =
    { enableColoredLabel : Bool
    , enableHideGiganticAndDoomDice : Bool
    , enableAddMissingDiceChoice : Bool
    , enableHelpOnTextInput : Bool
    , enableHelpOnDice : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        config =
            { enableColoredLabel = True
            , enableHideGiganticAndDoomDice = True
            , enableAddMissingDiceChoice = True
            , enableHelpOnTextInput = False
            , enableHelpOnDice = True
            }

        model =
            { attackDices = initialDiceChoice config
            , defenseDices = initialDiceChoice config
            , textInput = ""
            , attackResult = NoAttack
            , isConfigOpen = False
            , config = config
            }
    in
    ( model, Cmd.none )


initialDiceChoice : Config -> DiceChoice
initialDiceChoice config =
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


addMissingDiceChoice : Config -> DiceChoice -> DiceChoice
addMissingDiceChoice config diceChoice =
    if config.enableAddMissingDiceChoice then
        let
            isNotInside ( _, dice ) updated =
                List.isEmpty <| List.filter (\( _, d ) -> dice == d) updated

            f : ( Int, Dice ) -> DiceChoice -> DiceChoice
            f ( value, dice ) updated =
                if isNotInside ( value, dice ) updated then
                    updated ++ [ ( value, dice ) ]

                else
                    updateDiceChoice dice ((+) value) updated
        in
        List.foldr f (initialDiceChoice config) diceChoice

    else
        diceChoice



-- UPDATE


type
    Msg
    -- config panel
    = UserToggledConfigPanel
    | UserUpdatedConfig (Config -> Bool -> Config) Bool
      -- rolling dices
    | NewRollResult ( Roll, Roll )
    | UserPushedRollButton
    | UserPushedResetButton
      -- dice selection
    | UserTypedText String
    | UserUpdatedDiceChoice Bool Dice String
    | UserIncreasedDiceChoice Bool Dice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserToggledConfigPanel ->
            ( { model | isConfigOpen = not model.isConfigOpen }, Cmd.none )

        UserUpdatedConfig alter value ->
            ( { model | config = alter model.config value }, Cmd.none )

        NewRollResult ( attackResult, defenseResult ) ->
            let
                finalResult =
                    applyDefense attackResult defenseResult

                isSuccess =
                    finalResult == []
            in
            ( { model
                | attackResult =
                    AttackResult
                        { attackResult = attackResult
                        , defenseResult = defenseResult
                        , finalResult = finalResult
                        , isSuccess = isSuccess
                        }
              }
            , Cmd.none
            )

        UserPushedRollButton ->
            ( model
            , Random.generate NewRollResult (rollDicesSet model.attackDices model.defenseDices)
            )

        UserPushedResetButton ->
            let
                ( m, c ) =
                    init ()
            in
            ( { m | config = model.config }, c )

        UserTypedText value ->
            let
                ( attackDices, defenseDices, _ ) =
                    decodeDiceChoices value
            in
            ( { model
                | attackDices = addMissingDiceChoice model.config attackDices
                , defenseDices = addMissingDiceChoice model.config defenseDices
                , textInput = value
                , attackResult = NoAttack
              }
            , Cmd.none
            )

        UserUpdatedDiceChoice isAttackSection dice value ->
            updateDiceChoiceMessageHandler isAttackSection dice (\_ -> intOrZeroFromString value) model

        UserIncreasedDiceChoice isAttackSection dice ->
            updateDiceChoiceMessageHandler isAttackSection dice ((+) 1) model


updateDiceChoiceMessageHandler : Bool -> Dice -> (Int -> Int) -> Model -> ( Model, Cmd Msg )
updateDiceChoiceMessageHandler isAttackSection dice alter model =
    let
        newModel =
            if isAttackSection then
                { model | attackDices = updateDiceChoice dice alter model.attackDices }

            else
                { model | defenseDices = updateDiceChoice dice alter model.defenseDices }
    in
    ( { newModel
        | textInput = encodeDiceChoices ( newModel.attackDices, newModel.defenseDices )
        , attackResult = NoAttack
      }
    , Cmd.none
    )


updateDiceChoice : Dice -> (Int -> Int) -> DiceChoice -> DiceChoice
updateDiceChoice dice alter diceChoice =
    let
        f ( n, d ) =
            if d == dice then
                ( alter n, d )

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
            , if model.config.enableHelpOnTextInput then
                p [ class "help is-hidden-mobile" ] (capitalizeStrong "Black Red Yellow White Gigantic Doom")

              else
                text ""
            ]
        , viewConfigPanel model.config model.isConfigOpen
        , viewFinalResult model.attackResult
        ]


viewDiceSelectionAndResultSection : Model -> Bool -> Html Msg
viewDiceSelectionAndResultSection model isAttackSection =
    let
        ( attackResult, defenseResult ) =
            case model.attackResult of
                NoAttack ->
                    ( [], [] )

                AttackResult r ->
                    ( r.attackResult, r.defenseResult )

        x =
            if isAttackSection then
                { diceChoice = model.attackDices
                , result = attackResult
                , name = "Attack"
                , color = "has-background-link"
                }

            else
                { diceChoice = model.defenseDices
                , result = defenseResult
                , name = "Defense"
                , color = "has-background-primary"
                }
    in
    div [ class "column" ]
        [ div [ class <| "box is-hidden-mobile " ++ x.color ++ "-light" ]
            [ h2 [ class "title" ] [ text x.name ]
            , div [] (List.map (viewChosenDiceSelector model.config isAttackSection) x.diceChoice)
            ]
        , viewResult x.result "" x.color
        ]


viewFinalResult : AttackResult -> Html msg
viewFinalResult attackState =
    case attackState of
        NoAttack ->
            text ""

        AttackResult x ->
            viewResult x.finalResult "AttackFailed" "has-background-danger"


viewResult : Roll -> String -> String -> Html msg
viewResult result defaultText color =
    if result /= [] || defaultText /= "" then
        div [ class <| "box " ++ color ]
            [ h2 [ class "title has-text-white" ]
                (if result /= [] then
                    List.map (\f -> div [] [ text f ]) (printRoll result)

                 else
                    [ text "Attack failed" ]
                )
            ]

    else
        text ""


viewChosenDiceSelector : Config -> Bool -> ( Int, Dice ) -> Html Msg
viewChosenDiceSelector config isAttack ( n, dice ) =
    div [ class "field" ]
        [ viewColoredDiceLabel config isAttack ( n, dice )
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
        , if config.enableHelpOnDice then
            p [ class "help" ] [ text <| printDice dice ]

          else
            text ""
        ]


viewColoredDiceLabel : Config -> Bool -> ( Int, Dice ) -> Html Msg
viewColoredDiceLabel config isAttack ( n, dice ) =
    if config.enableColoredLabel then
        label [ class "label", onClick (UserIncreasedDiceChoice isAttack dice) ]
            [ i
                [ class <| diceIcon n
                , style "color" dice.color
                , style "text-shadow" "-2px 2px 4px Silver, 2px -2px 0 White"
                ]
                []
            , text <| " " ++ dice.name
            ]

    else
        label [ class "label", onClick (UserIncreasedDiceChoice isAttack dice) ] [ text dice.name ]


viewConfigPanel : Config -> Bool -> Html Msg
viewConfigPanel config isOpen =
    if isOpen then
        div [ class "block box has-background-warning-light" ]
            [ viewConfigItem
                config.enableColoredLabel
                (\c b -> { c | enableColoredLabel = b })
                " Colored dice label"
            , viewConfigItem
                config.enableHelpOnTextInput
                (\c b -> { c | enableHelpOnTextInput = b })
                " Show help on text input"
            , viewConfigItem
                config.enableHelpOnDice
                (\c b -> { c | enableHelpOnDice = b })
                " Show dice faces"
            , viewConfigItem
                config.enableAddMissingDiceChoice
                (\c b -> { c | enableAddMissingDiceChoice = b })
                " Show all dice when typing"
            , viewConfigItem
                config.enableHideGiganticAndDoomDice
                (\c b -> { c | enableHideGiganticAndDoomDice = b })
                " & hide Gigantic and Doom dice"
            ]

    else
        text ""


viewConfigItem : Bool -> (Config -> Bool -> Config) -> String -> Html Msg
viewConfigItem value alter description =
    div [ class "field" ]
        [ div [ class "control" ]
            [ label [ class "checkbox" ]
                [ input [ type_ "checkbox", checked value, onCheck (UserUpdatedConfig alter) ] []
                , text description
                ]
            ]
        ]


viewHeader : () -> Html msg
viewHeader () =
    div [ class "block" ]
        [ h1 [ class "title is-1" ] [ text "JoA Dice" ]
        , h1 [ class "subtitle is-hidden-mobile" ]
            [ text "a helper for "
            , a [ href "https://boardgamegeek.com/boardgame/230791/time-legends-joan-arc" ]
                [ text "Time of Legends: Joan of Arc" ]
            ]
        ]


viewFooter : () -> Html Msg
viewFooter () =
    footer [ class "footer is-hidden-mobile" ]
        [ div [ class "has-text-centered" ]
            [ span [ onClick UserToggledConfigPanel ] [ text <| "made with " ++ heartString ++ ", " ]
            , a [ href "https://elm-lang.org" ] [ text "elm" ]
            , text " and "
            , a [ href "https://bulma.io" ] [ text "bulma" ]
            , p []
                [ a [ href "https://github.com/topics/joa-dices-rewrite" ] [ text "joa-dices-rewrite" ]
                , text " collection 2021"
                ]
            ]
        ]



-- UTILS


intOrZeroFromString : String -> Int
intOrZeroFromString value =
    Maybe.withDefault 0 (String.toInt value)


heartString : String
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


diceIcon : Int -> String
diceIcon n =
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


capitalizeStrong : String -> List (Html msg)
capitalizeStrong string =
    String.split " " string
        |> List.concatMap
            (\s ->
                [ span [] [ text <| String.left 1 s ]
                , span [ class "has-text-grey" ] [ text <| String.dropLeft 1 s ]
                , text " "
                ]
            )
