module Ecs.Entity exposing (Entity, newEntity)

import Uuid exposing (Uuid)



-- Entity


type alias Entity =
    Uuid


newEntity : Uuid -> Entity
newEntity id =
    id
