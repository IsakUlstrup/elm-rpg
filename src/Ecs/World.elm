module Ecs.World exposing
    ( World
    , addComponent
    , addEntity
    , enabledEntityComponents
    , enabledFilter
    , entityComponents
    , hasComponentData
    , newWorld
    , removeComponent
    , removeEntity
    , setSeed
    , toggleComponent
    , updateComponent
    )

import ComponentData exposing (ComponentData)
import Ecs.Component exposing (Component)
import Ecs.Entity exposing (Entity)
import Random
import Uuid exposing (uuidGenerator)



-- World


type alias World =
    { name : String
    , entities : List Entity
    , components : List Component
    , seed : Random.Seed
    }


newWorld : Random.Seed -> String -> World
newWorld seed name =
    World name [] [] seed


setSeed : Random.Seed -> World -> World
setSeed seed world =
    { world | seed = seed }


addEntity : World -> World
addEntity world =
    let
        ( id, seed ) =
            Random.step uuidGenerator world.seed

        entity =
            Ecs.Entity.newEntity id
    in
    { world
        | entities = entity :: world.entities
        , seed = seed
    }


entityComponents : World -> Entity -> List Component
entityComponents world entity =
    List.filter (\c -> c.parent == entity) world.components


enabledEntityComponents : World -> Entity -> List Component
enabledEntityComponents world entity =
    entityComponents world entity
        |> List.filter enabledFilter



-- remove entity and any attached components


removeEntity : World -> Entity -> World
removeEntity world entity =
    { world
        | entities = List.filter (\e -> e /= entity) world.entities
        , components = List.filter (\c -> c.parent /= entity) world.components
    }


removeComponent : World -> Component -> World
removeComponent world component =
    { world | components = List.filter (\c -> c.id /= component.id) world.components }


toggleComponent : World -> Component -> World
toggleComponent world component =
    { world
        | components =
            List.map
                (\c ->
                    if c.id == component.id then
                        { c | enabled = not c.enabled }

                    else
                        c
                )
                world.components
    }


updateComponent : World -> Component -> World
updateComponent world component =
    { world
        | components =
            List.map
                (\c ->
                    if c.id == component.id then
                        { c | data = component.data }

                    else
                        c
                )
                world.components
    }


enabledFilter : Component -> Bool
enabledFilter component =
    component.enabled


hasComponentData : ComponentData -> World -> Entity -> Bool
hasComponentData data world entity =
    world.components
        |> List.filter enabledFilter
        |> List.filter (\comp -> comp.parent == entity)
        |> List.map (\c -> c.data)
        |> List.member data



-- component.data == data


addComponent : ComponentData -> Entity -> World -> World
addComponent componentData parent world =
    let
        ( id, seed ) =
            Random.step uuidGenerator world.seed
    in
    { world
        | components =
            Ecs.Component.newComponent id parent componentData :: world.components
        , seed = seed
    }
