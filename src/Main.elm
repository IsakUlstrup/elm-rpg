module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyPress)
import ComponentData exposing (ComponentData(..), newStatusEffectComponentData)
import Ecs.Component exposing (Component)
import Ecs.Editor exposing (EditorMsg(..))
import Ecs.Entity exposing (Entity)
import Ecs.World exposing (World)
import Html
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Random
import Renderer exposing (GameMsg(..))
import Skill exposing (SkillEffect(..))
import SkillSystem exposing (skillSystem)
import Stat
import StatusEffect exposing (StatusEffectData, StatusEffectDuration(..), reduceDuration)
import Uuid exposing (Uuid)



-- test stuff


getStatusEffects : Entity -> World -> List StatusEffectData
getStatusEffects entity world =
    world.components
        |> List.filter (\c -> c.enabled)
        |> List.filter (\comp -> comp.parent == entity)
        |> List.map (\c -> c.data)
        |> List.filterMap
            (\a ->
                case a of
                    ComponentData.StatusEffect eff ->
                        Just eff

                    _ ->
                        Nothing
            )


energySystem : Float -> World -> World
energySystem dt world =
    let
        baseEnergyRegen =
            0.003

        parentEnergyRegenModifier parent =
            StatusEffect.getStatOfType (getStatusEffects parent world) Stat.EnergyRegen

        energyRegen amount component =
            case component.data of
                Skill skill ->
                    { component
                        | data =
                            Skill
                                { skill
                                    | energy =
                                        min skill.energyUse
                                            (skill.energy
                                                + (amount
                                                    * (parentEnergyRegenModifier component.parent
                                                        |> Maybe.withDefault 0.1
                                                      )
                                                  )
                                            )
                                }
                    }

                _ ->
                    component
    in
    { world | components = List.map (energyRegen (dt * baseEnergyRegen)) world.components }


targetSystem : Float -> World -> World
targetSystem _ world =
    let
        validTargets : Uuid -> List Entity -> List Entity
        validTargets parent entities =
            let
                entityIsPlayer : Entity -> Bool
                entityIsPlayer entity =
                    Ecs.World.hasComponentData ComponentData.newPlayerComponentData world entity
            in
            if entityIsPlayer parent then
                List.filter (\e -> e /= parent && not (entityIsPlayer e)) entities

            else
                List.filter (\e -> e /= parent && entityIsPlayer e) entities

        findTarget : Component -> Component
        findTarget component =
            case component.data of
                Skill skill ->
                    { component
                        | data =
                            Skill
                                { skill
                                    | target =
                                        validTargets component.parent world.entities
                                            |> List.reverse
                                            |> List.head
                                }
                    }

                _ ->
                    component
    in
    { world | components = List.map findTarget world.components }


statusEffectSystem : Float -> World -> World
statusEffectSystem dt world =
    { world
        | components =
            List.filterMap
                (\component ->
                    case component.data of
                        ComponentData.StatusEffect effect ->
                            reduceDuration effect dt
                                |> (\effectData ->
                                        case effectData.duration of
                                            Remaining time ->
                                                if time > 0 then
                                                    Just { component | data = newStatusEffectComponentData (reduceDuration effect dt) }

                                                else
                                                    Nothing

                                            Unlimited ->
                                                Just { component | data = newStatusEffectComponentData (reduceDuration effect dt) }
                                   )

                        -- { component | data = newStatusEffectComponentData (reduceDuration effect dt) }
                        _ ->
                            Just component
                )
                world.components
    }


getHealth : Entity -> World -> Maybe Float
getHealth entity world =
    Ecs.World.enabledEntityComponents world entity
        |> List.map (\c -> c.data)
        |> ComponentData.getHealth


processEntity : List Entity -> World -> ( List Entity, World )
processEntity entities world =
    case entities of
        [] ->
            ( entities, world )

        x :: xs ->
            case getHealth x world of
                Just health ->
                    if health <= 0 then
                        processEntity xs (Ecs.World.removeEntity world x)

                    else
                        processEntity xs world

                _ ->
                    processEntity xs world


deathSystem : Float -> World -> World
deathSystem _ world =
    Tuple.second (processEntity world.entities world)



-- Model


type alias Model =
    { world : World
    , dt : Float
    , showEditor : Bool
    }


newModel : Int -> Model
newModel initialTimestamp =
    Model
        (Ecs.World.newWorld
            (Random.initialSeed initialTimestamp)
            "Test World"
        )
        0
        True


keyDecoder : Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            CharacterKey ' '


type Msg
    = Tick Float
    | Editor Ecs.Editor.EditorMsg
    | Game Renderer.GameMsg
    | CharacterKey Char



-- Init


init : Int -> ( Model, Cmd Msg )
init initialTime =
    ( newModel initialTime
    , Cmd.none
    )



-- Subs


subs : a -> Sub Msg
subs _ =
    Sub.batch
        [ onKeyPress keyDecoder
        , onAnimationFrameDelta Tick
        ]



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            { model
                | dt = dt
                , world =
                    model.world
                        |> energySystem dt
                        |> targetSystem dt
                        |> skillSystem dt
                        |> statusEffectSystem dt
                        |> deathSystem dt

                -- , timestampCounter = model.timestampCounter + 1
                -- , world = World.setSeed (Random.initialSeed model.timestampCounter) model.world
                -- , dtHistory = dt :: List.take 20 model.dtHistory
            }

        CharacterKey key ->
            if key == '<' then
                { model | showEditor = not model.showEditor }

            else
                model

        Editor editorMsg ->
            case editorMsg of
                AddEntity ->
                    { model
                        | world = Ecs.World.addEntity model.world
                    }

                AddComponent parent data ->
                    { model
                        | world = Ecs.World.addComponent data parent model.world
                    }

                RemoveComponent component ->
                    { model
                        | world = Ecs.World.removeComponent model.world component
                    }

                RemoveEntity entity ->
                    { model
                        | world = Ecs.World.removeEntity model.world entity
                    }

                ToggleComponent component ->
                    { model
                        | world = Ecs.World.toggleComponent model.world component
                    }

                UpdateComponent component ->
                    { model
                        | world = Ecs.World.updateComponent model.world component
                    }

        Game gameMsg ->
            case gameMsg of
                ToggleSkill component ->
                    { model
                        | world =
                            Ecs.World.updateComponent model.world
                                { component | data = ComponentData.toggleSkill component.data }
                    }



-- Main


main : Program Int Model Msg
main =
    Browser.element
        { view =
            \model ->
                Html.div [ Html.Attributes.id "app" ]
                    [ if model.showEditor then
                        Html.map
                            Editor
                            (Ecs.Editor.render
                                model.world
                            )

                      else
                        Html.text ""
                    , Html.map Game (Renderer.render model.world)
                    ]
        , update = \msg model -> ( update msg model, Cmd.none )
        , init = init
        , subscriptions = subs
        }
