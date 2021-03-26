module EnergySystem exposing (energySystem)

import ComponentData exposing (ComponentData(..))
import Ecs.Entity exposing (Entity)
import Ecs.World exposing (World)
import Stat
import StatusEffect exposing (StatusEffectData)


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
