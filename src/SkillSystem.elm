module SkillSystem exposing (skillSystem)

import ComponentData exposing (ComponentData(..))
import Ecs.Component exposing (Component)
import Ecs.Entity exposing (Entity)
import Ecs.World exposing (World)
import Skill


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
    { world
        | components =
            List.map
                (\c ->
                    if c.parent == target then
                        dealDamage damage c

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
processSkill components wrld =
    case components of
        [] ->
            ( components, wrld )

        x :: xs ->
            case x.data of
                Skill skill ->
                    if skill.energy >= skill.energyUse && skill.autoUse && maybeBool skill.target then
                        Ecs.World.updateComponent wrld { x | data = Skill { skill | energy = 0 } }
                            |> (\world2 ->
                                    List.foldl
                                        (processSkillEffect skill)
                                        world2
                                        skill.effects
                               )
                            |> processSkill xs

                    else
                        processSkill xs wrld

                _ ->
                    processSkill xs wrld


skillSystem : Float -> World -> World
skillSystem _ world =
    Tuple.second (processSkill world.components world)
