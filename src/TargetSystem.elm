module TargetSystem exposing (targetSystem)

import ComponentData exposing (ComponentData(..))
import Ecs.Component exposing (Component)
import Ecs.Entity exposing (Entity)
import Ecs.World exposing (World)
import Uuid exposing (Uuid)


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
