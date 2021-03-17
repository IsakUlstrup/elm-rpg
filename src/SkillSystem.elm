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


dealDamage : Entity -> Float -> World -> World
dealDamage target damage world =
    { world
        | components =
            List.map
                (\c ->
                    if c.parent == target then
                        case c.data of
                            ComponentData.Health health ->
                                { c
                                    | data =
                                        Health
                                            (health
                                                - (damage
                                                    / (Ecs.World.enabledEntityComponents world target
                                                        |> List.map (\comp -> comp.data)
                                                        |> ComponentData.getHealth
                                                        |> Maybe.withDefault 1
                                                      )
                                                  )
                                            )
                                }

                            _ ->
                                c

                    else
                        c
                )
                world.components
    }


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
                                        (\effect wr ->
                                            case skill.target of
                                                Just target ->
                                                    case effect of
                                                        Skill.Damage damage ->
                                                            dealDamage target damage wr

                                                        Skill.StatusEffect statusEffect ->
                                                            Ecs.World.addComponent (ComponentData.newStatusEffectComponentData statusEffect) target wr

                                                _ ->
                                                    wr
                                        )
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
