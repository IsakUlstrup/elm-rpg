module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyPress)
import ComponentData exposing (ComponentData(..))
import Ecs.Editor exposing (EditorMsg(..))
import Ecs.Entity exposing (Entity)
import Ecs.World exposing (World)
import Html
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Random
import Renderer exposing (GameMsg(..))
import Stat
import StatusEffect exposing (StatusEffectData)



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
                    StatusEffect eff ->
                        Just eff

                    _ ->
                        Nothing
            )


energySystem : Float -> World -> World
energySystem dt world =
    let
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
    { world | components = List.map (energyRegen (dt * 0.003)) world.components }


skillSystem : Float -> World -> World
skillSystem _ world =
    let
        useSkill component =
            case component.data of
                Skill skill ->
                    if skill.energy >= skill.energyUse && skill.autoUse then
                        { component
                            | data =
                                Skill
                                    { skill
                                        | energy = 0
                                    }
                        }

                    else
                        component

                _ ->
                    component
    in
    { world | components = List.map useSkill world.components }



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
                        |> skillSystem dt

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
