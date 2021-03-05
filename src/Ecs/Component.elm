module Ecs.Component exposing (Component, newComponent)

import ComponentData exposing (ComponentData)
import Ecs.Entity exposing (Entity)
import Uuid exposing (Uuid)



-- Component


type alias Component =
    { id : Uuid
    , enabled : Bool
    , parent : Uuid
    , data : ComponentData
    }


newComponent : Uuid -> Entity -> ComponentData -> Component
newComponent id parent data =
    Component id True parent data
