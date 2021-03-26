module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyPress)
import ComponentData exposing (ComponentData(..))
import DeathSystem exposing (deathSystem)
import Ecs.Editor exposing (EditorMsg(..))
import Ecs.World exposing (World)
import EnergySystem exposing (energySystem)
import Html
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Random
import Renderer exposing (GameMsg(..))
import Skill exposing (SkillEffect(..))
import SkillSystem exposing (skillSystem)
import StatusEffect exposing (StatusEffectDuration(..))
import StatusEffectSystem exposing (statusEffectSystem)
import TargetSystem exposing (targetSystem)


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
