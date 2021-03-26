module StatusEffectSystem exposing (statusEffectSystem)

import ComponentData exposing (newStatusEffectComponentData)
import Ecs.World exposing (World)
import StatusEffect exposing (StatusEffectDuration(..), reduceDuration)


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
