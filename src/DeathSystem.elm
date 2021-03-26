module DeathSystem exposing (deathSystem)

import ComponentData
import Ecs.Entity exposing (Entity)
import Ecs.World exposing (World)


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
