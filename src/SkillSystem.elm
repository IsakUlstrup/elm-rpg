module SkillSystem exposing (skillSystem)

import ComponentData exposing (ComponentData(..))
import Ecs.Component exposing (Component)
import Ecs.Entity exposing (Entity)
import Ecs.World exposing (World)
import Skill
import Stat exposing (StatType)
import StatusEffect exposing (StatusEffectData)


maybeBool : Maybe a -> Bool
maybeBool mby =
    case mby of
        Just _ ->
            True

        Nothing ->
            False


dealDamage : Float -> Component -> Component
dealDamage damage component =
    case component.data of
        ComponentData.Health health ->
            { component
                | data =
                    Health
                        (health
                            - (damage
                               -- / (Ecs.World.enabledEntityComponents world target
                               --     |> List.map (\comp -> comp.data)
                               --     |> ComponentData.getHealth
                               --     |> Maybe.withDefault 1
                               --   )
                              )
                        )
            }

        _ ->
            component


processDamage : Entity -> Float -> World -> World
processDamage target damage world =
    let
        getStat : Entity -> World -> StatType -> Maybe Float
        getStat entity w statType =
            StatusEffect.getStatOfType (getStatusEffects entity w) statType

        getStatusEffects : Entity -> World -> List StatusEffectData
        getStatusEffects entity w =
            Ecs.World.enabledEntityComponents w entity
                |> List.map (\c -> c.data)
                |> List.filterMap
                    (\a ->
                        case a of
                            StatusEffect eff ->
                                Just eff

                            _ ->
                                Nothing
                    )
    in
    { world
        | components =
            List.map
                (\c ->
                    if c.parent == target then
                        dealDamage
                            (damage / (getStat target world Stat.Health |> Maybe.withDefault 1))
                            c

                    else
                        c
                )
                world.components
    }


processSkillEffect : Skill.SkillData -> Skill.SkillEffect -> World -> World
processSkillEffect skill effect world =
    case skill.target of
        Just target ->
            case effect of
                Skill.Damage damage ->
                    processDamage target damage world

                Skill.StatusEffect statusEffect ->
                    Ecs.World.addComponent (ComponentData.newStatusEffectComponentData statusEffect) target world

        _ ->
            world


processSkill : List Component -> World -> ( List Component, World )
processSkill components world =
    case components of
        [] ->
            ( components, world )

        x :: xs ->
            case x.data of
                Skill skill ->
                    if skill.energy >= skill.energyUse && skill.autoUse && maybeBool skill.target then
                        Ecs.World.updateComponent world { x | data = Skill { skill | energy = 0 } }
                            |> (\w ->
                                    List.foldl
                                        (processSkillEffect skill)
                                        w
                                        skill.effects
                               )
                            |> processSkill xs

                    else
                        processSkill xs world

                _ ->
                    processSkill xs world


skillSystem : Float -> World -> World
skillSystem _ world =
    Tuple.second (processSkill world.components world)
