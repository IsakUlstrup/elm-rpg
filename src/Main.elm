module Main exposing (Encounter, Model, Msg, main)

import Browser
import Browser.Events
import Character exposing (Character, isDead)
import Content.Characters
import Encounters.AutoCounter as AutoCounter exposing (AutoCounter, Msg(..))
import Encounters.Combat exposing (Combat, Msg(..), tickModel)
import Encounters.Counter as Counter exposing (Counter)
import Encounters.Debug
import Html exposing (Attribute, Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Map exposing (Direction(..), Map, Position)
import Meter exposing (Meter, filledPercentage)
import Random
import Skill exposing (Skill)
import Turn



-- ENCOUNTER


type Encounter
    = CounterEncounter Counter
    | AutoCounterEncounter AutoCounter
    | Debug
    | CombatEncounter Combat


tickEncounter : Float -> Position -> Encounter -> Encounter
tickEncounter dt _ encounter =
    case encounter of
        AutoCounterEncounter counter ->
            counter |> AutoCounter.tick dt |> AutoCounterEncounter

        CombatEncounter combat ->
            combat |> tickModel dt |> CombatEncounter

        _ ->
            encounter



-- MODEL


type alias Model =
    { player : Character
    , seed : Random.Seed
    , map : Map Encounter
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Characters.hero
        (Random.initialSeed 32)
        (Map.empty
            |> Map.addEntity ( 0, 0, 0 ) Debug
            |> Map.addEntity ( -1, 0, 0 ) Debug
            |> Map.addEntity ( 0, -1, 0 ) (CounterEncounter Counter.zero)
            |> Map.addEntity ( -1, -1, 0 ) (AutoCounterEncounter (AutoCounter.new 10000))
            |> Map.addEntity ( 1, 0, 0 ) (CombatEncounter (Combat Content.Characters.ghost Turn.playerIdle))
        )
    , Cmd.none
    )


setCurrentEntity : Encounter -> Model -> Model
setCurrentEntity entity model =
    { model
        | map = Map.setCurrentEntity entity model.map
    }



-- UPDATE


type Msg
    = Tick Float
    | ClickedMoveMap Direction
    | CounterMsg Counter.Msg
    | AutoCounterMsg AutoCounter.Msg
    | DebugMsg Encounters.Debug.Msg
    | CombatMsg Encounters.Combat.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, Map.getCurrentEntity model.map ) of
        ( Tick dt, _ ) ->
            ( { model
                | map =
                    model.map
                        |> Map.tick dt
                        |> Map.updateEntities (tickEncounter dt)
              }
            , Cmd.none
            )

        ( ClickedMoveMap direction, _ ) ->
            ( { model | map = Map.move direction model.map }
            , Cmd.none
            )

        ( CounterMsg Counter.ClickedIncrement, Just (CounterEncounter counter) ) ->
            ( setCurrentEntity
                (counter
                    |> Counter.increment
                    |> CounterEncounter
                )
                model
            , Cmd.none
            )

        ( CounterMsg Counter.ClickedDecrement, Just (CounterEncounter counter) ) ->
            ( setCurrentEntity
                (counter
                    |> Counter.decrement
                    |> CounterEncounter
                )
                model
            , Cmd.none
            )

        ( CounterMsg Counter.ClickedReset, Just (CounterEncounter _) ) ->
            ( setCurrentEntity
                (CounterEncounter Counter.zero)
                model
            , Cmd.none
            )

        ( AutoCounterMsg AutoCounter.ClickedReset, Just (AutoCounterEncounter counter) ) ->
            ( setCurrentEntity
                (counter
                    |> AutoCounter.reset
                    |> AutoCounterEncounter
                )
                model
            , Cmd.none
            )

        ( DebugMsg Encounters.Debug.ClickedRestoreHealth, _ ) ->
            ( { model | player = Character.heal 100 model.player }
            , Cmd.none
            )

        ( CombatMsg (ClickedPlayerSkill index), Just (CombatEncounter combat) ) ->
            ( case model.player.skills |> List.drop index |> List.head of
                Just skill ->
                    let
                        ( hitRoll, newSeed ) =
                            Random.step (Character.rollSkill skill model.player combat.enemy) model.seed
                    in
                    setCurrentEntity
                        (CombatEncounter
                            { combat
                                | turn = Turn.attacking True
                                , enemy = Character.hit hitRoll combat.enemy
                            }
                        )
                        { model | seed = newSeed }

                Nothing ->
                    model
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )



-- VIEW


viewTrailMeter : Meter -> Html msg
viewTrailMeter meter =
    let
        ( trailDelay, valueDelay ) =
            if meter.headingDown then
                ( 200, 0 )

            else
                ( 0, 200 )
    in
    Html.div [ Html.Attributes.class "meter" ]
        [ Html.div
            [ Html.Attributes.class "trail"
            , Html.Attributes.style "width" (String.fromInt (filledPercentage meter) ++ "%")
            , Html.Attributes.style "transition-delay" (String.fromInt trailDelay ++ "ms")
            ]
            []
        , Html.div
            [ Html.Attributes.class "value"
            , Html.Attributes.style "width" (String.fromInt (filledPercentage meter) ++ "%")
            , Html.Attributes.style "transition-delay" (String.fromInt valueDelay ++ "ms")
            ]
            []
        ]


viewSkill : List (Attribute msg) -> List (Html msg) -> Skill -> Html msg
viewSkill attrs children skill =
    Html.div
        (Html.Attributes.class "skill"
            :: attrs
        )
        (Html.p [] [ Html.text skill.name ]
            :: children
        )


viewCharacter : Character -> List (Attribute msg) -> List (Html msg) -> Html msg
viewCharacter character attrs children =
    Html.div
        ([ Html.Attributes.class "character"
         , Html.Attributes.attribute "style" ("--animation-duration: " ++ (String.fromInt Turn.duration ++ "ms"))
         ]
            ++ attrs
        )
        ([ Html.h1 [] [ Html.text character.name ]
         , Html.div [ Html.Attributes.class "health-history" ]
            (character.healthHistory
                |> List.map
                    (\h ->
                        Html.p [ Html.Attributes.class "history-item" ] [ String.fromInt h |> Html.text ]
                    )
            )
         , viewTrailMeter character.health
         ]
            ++ children
        )


viewCombat : Character -> Combat -> Html Encounters.Combat.Msg
viewCombat player model =
    Html.div []
        [ viewCharacter model.enemy
            [ Html.Attributes.classList
                [ ( "enemy", True )
                , ( "active", Turn.isEnemyTurn model.turn )
                , ( "hurt", Turn.isTakingDamage False model.turn )
                , ( "attacking", Turn.isAttacking False model.turn )
                , ( "dead", isDead model.enemy )
                ]
            ]
            [ Html.div [ Html.Attributes.class "skills" ] (model.enemy.skills |> List.take 1 |> List.map (viewSkill [] []))
            ]
        , viewCharacter player
            [ Html.Attributes.classList
                [ ( "player", True )
                , ( "active", Turn.isPlayerTurn model.turn )
                , ( "hurt", Turn.isTakingDamage True model.turn )
                , ( "attacking", Turn.isAttacking True model.turn )
                , ( "dead", isDead player )
                ]
            ]
            [ Html.div [ Html.Attributes.class "skills" ]
                (List.indexedMap
                    (\index skill ->
                        viewSkill
                            [ Html.Events.onClick (ClickedPlayerSkill index)
                            ]
                            []
                            skill
                    )
                    player.skills
                )
            ]
        ]


viewEncounter : Character -> Position -> Encounter -> Html Msg
viewEncounter player position encounter =
    Html.div [ Html.Attributes.class "encounter" ]
        [ Html.h3 [] [ Html.text (Map.positionToString position) ]
        , case encounter of
            CounterEncounter counter ->
                Html.map CounterMsg (Counter.view counter)

            AutoCounterEncounter counter ->
                Html.map AutoCounterMsg (AutoCounter.view counter)

            CombatEncounter combat ->
                Html.map CombatMsg (viewCombat player combat)

            Debug ->
                Html.map DebugMsg Encounters.Debug.view
        ]


view : Model -> Html Msg
view model =
    main_
        [ Html.Attributes.id "game"
        ]
        [ Map.view (viewEncounter model.player) model.map
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown directionDecoder
        ]


directionDecoder : Decoder Msg
directionDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "w" ->
                        Decode.succeed (ClickedMoveMap North)

                    "ArrowUp" ->
                        Decode.succeed (ClickedMoveMap North)

                    "s" ->
                        Decode.succeed (ClickedMoveMap South)

                    "ArrowDown" ->
                        Decode.succeed (ClickedMoveMap South)

                    "a" ->
                        Decode.succeed (ClickedMoveMap West)

                    "ArrowLeft" ->
                        Decode.succeed (ClickedMoveMap West)

                    "d" ->
                        Decode.succeed (ClickedMoveMap East)

                    "ArrowRight" ->
                        Decode.succeed (ClickedMoveMap East)

                    _ ->
                        Decode.fail "unknown key"
            )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
