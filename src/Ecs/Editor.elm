module Ecs.Editor exposing
    ( EditorMsg(..)
    , render
    )

import ComponentData exposing (ComponentData)
import Ecs.Component exposing (Component)
import Ecs.Entity exposing (Entity)
import Ecs.World exposing (World)
import Html exposing (Html, br, h3, header, input, label, li, p, section, strong, table, td, text, tr, ul)
import Html.Attributes exposing (checked, class, for, id, type_)
import Html.Events exposing (onClick, onInput)
import Uuid


type EditorMsg
    = AddEntity
    | AddComponent Entity ComponentData
    | RemoveEntity Entity
    | RemoveComponent Component
    | ToggleComponent Component
    | UpdateComponent Component


worldStats : World -> Html msg
worldStats world =
    header []
        [ h3 [] [ text world.name ]
        , table []
            [ tr []
                [ td []
                    [ text "Seed" ]
                , td []
                    [ text (Debug.toString world.seed) ]
                ]
            , tr []
                [ td []
                    [ text "Entities" ]
                , td []
                    [ text (String.fromInt (List.length world.entities)) ]
                ]
            , tr []
                [ td []
                    [ text "Components" ]
                , td []
                    [ text (String.fromInt (List.length world.components)) ]
                ]
            , tr []
                [ td []
                    [ text "Systems" ]
                , td []
                    [ text "TBA" ]
                ]
            ]
        ]


entity : World -> Entity -> Html EditorMsg
entity world e =
    let
        addComponentButton tuple =
            button (Tuple.first tuple) (AddComponent e (Tuple.second tuple))
    in
    li [ class "entity" ]
        [ strong [] [ button "-" (RemoveEntity e), text (Uuid.toString e) ]
        , br []
            []
        , p
            []
            (List.map
                addComponentButton
                ComponentData.defaultData
            )
        , componentList (Ecs.World.entityComponents world e)
        ]


component : Component -> Html EditorMsg
component c =
    li [ class "component" ]
        [ input
            [ type_ "checkbox"
            , onClick (ToggleComponent c)
            , checked c.enabled
            , id (Uuid.toString c.id)
            ]
            []
        , label [ for (Uuid.toString c.id) ] [ text (ComponentData.componentTypeString c.data) ]
        , button "-" (RemoveComponent c)

        -- , br [] []
        -- , text (Debug.toString c.data)
        , br [] []
        , table [ class "component-attributes" ]
            (List.map
                (\data ->
                    tr []
                        [ td [] [ text data.label ]
                        , td []
                            [ data.element
                                (onInput (\input -> UpdateComponent { c | data = data.update input })
                                    :: data.attributes
                                )
                                data.children
                            ]
                        ]
                )
                (ComponentData.editComponentData c.data)
            )
        ]


entityList : World -> Html EditorMsg
entityList world =
    h3 [] [ text ("Entities (" ++ String.fromInt (List.length world.entities) ++ ")"), button "+" AddEntity ]
        :: List.map (entity world) world.entities
        |> ul [ class "entity-list" ]


componentList : List Component -> Html EditorMsg
componentList components =
    List.map component components
        |> ul [ class "component-list" ]


button : String -> EditorMsg -> Html EditorMsg
button label messageType =
    Html.button [ onClick messageType, class "button" ] [ Html.text label ]


render : World -> Html EditorMsg
render world =
    section [ id "editor" ]
        [ worldStats world
        , entityList world

        -- , componentList world.components
        ]
