module Engine.Path exposing (Node, Path, pathfind)

import Dict exposing (Dict)
import Engine.Point as Point exposing (Point)


type alias Node =
    { g : Int
    , h : Int
    , parent : Point
    }


{-| Derive f cost of node
-}
f : Node -> Int
f node =
    node.g + node.h


type alias Path =
    { open : Dict Point Node
    , closed : Dict Point Node
    , from : Point
    , to : Point
    , path : Maybe (List Point)
    }


{-| Initialize path with start point in open list
-}
init : Point -> Point -> Path
init from to =
    Path (Dict.singleton from (Node 0 0 from)) Dict.empty from to Nothing


{-| Find shortest path between two points using A\*
-}
pathfind : (Point -> Point -> Bool) -> Point -> Point -> Path
pathfind canMove from to =
    init from to |> step canMove


{-| Move node from open to closed
-}
moveToClosed : ( Point, Node ) -> Path -> Path
moveToClosed ( position, node ) path =
    { path
        | open = path.open |> Dict.remove position
        , closed = path.closed |> Dict.insert position node
    }


{-| Find the cheapest node to move to
-}
findCheapest : Dict Point Node -> Maybe ( Point, Node )
findCheapest nodes =
    let
        -- Find cheapest node
        cheapest : Maybe Int
        cheapest =
            nodes
                |> Dict.toList
                |> List.map (Tuple.second >> f)
                |> List.sort
                |> List.head
    in
    cheapest
        |> Maybe.andThen
            (\c ->
                -- of all cheapest nodes, sort by h value and return first
                nodes
                    |> Dict.toList
                    |> List.filter (\( _, n ) -> f n == c)
                    |> List.sortBy (Tuple.second >> .h)
                    |> List.head
            )


{-| Update mnode cost and parent to those from newNode
-}
updateCost : Node -> Maybe Node -> Maybe Node
updateCost newNode mnode =
    Maybe.map
        (\n4 ->
            { n4
                | g = newNode.g
                , h = newNode.h
                , parent = newNode.parent
            }
        )
        mnode


{-| Find neighbouring points that can be moved to and are not in closed list, add them to open list
-}
addNeighboursToOpen : (Point -> Point -> Bool) -> ( Point, Node ) -> Path -> Path
addNeighboursToOpen canMove ( position, node ) path =
    let
        neighbours : List ( Point, Node )
        neighbours =
            Point.neighbours position
                |> List.filter (\p -> (Dict.member p path.closed |> not) && canMove position p)
                |> List.map
                    (\p ->
                        ( p, Node (node.g + 1) (Point.distance path.to p) position )
                    )

        open : Dict Point Node
        open =
            List.foldl
                (\( p, n ) openNodes ->
                    case Dict.get p openNodes of
                        Just n2 ->
                            if n.g < n2.g then
                                Dict.update p (updateCost n) openNodes

                            else
                                openNodes

                        Nothing ->
                            Dict.insert p n openNodes
                )
                path.open
                neighbours
    in
    { path | open = open }


reconstructPath : ( Point, Node ) -> Dict Point Node -> List Point -> List Point
reconstructPath ( position, node ) nodes acum =
    if node.parent == position then
        position :: acum

    else
        case Dict.get node.parent nodes of
            Just parent ->
                position
                    :: acum
                    |> reconstructPath ( node.parent, parent ) nodes

            Nothing ->
                position :: acum


{-| Main pathfinding loop
-}
step : (Point -> Point -> Bool) -> Path -> Path
step canMove path =
    case ( findCheapest path.open, Dict.member path.to path.closed ) of
        ( Just cheapest, False ) ->
            if Tuple.first cheapest == path.to then
                { path | path = Just (reconstructPath cheapest path.closed []) }

            else
                path
                    |> addNeighboursToOpen canMove cheapest
                    |> moveToClosed cheapest
                    |> step canMove

        _ ->
            path
