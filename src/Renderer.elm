module Renderer exposing (GameMsg(..), render)

import ComponentData exposing (ComponentData(..))
import Ecs.Component exposing (Component)
import Ecs.Entity exposing (Entity)
import Ecs.World exposing (World)
import Html exposing (Html, aside, br, button, div, h1, li, meter, p, section, strong, text, ul)
import Html.Attributes exposing (class, id, step, value)
import Html.Events exposing (onClick)
import Skill exposing (SkillData)
import Stat exposing (StatType)
import StatusEffect exposing (StatusEffectData)


type GameMsg
    = ToggleSkill Component


getStat : Entity -> World -> StatType -> Maybe Float
getStat entity world statType =
    StatusEffect.getStatOfType (getStatusEffects entity world) statType


getName : Entity -> World -> Maybe String
getName entity world =
    Ecs.World.enabledEntityComponents world entity
        |> List.map (\c -> c.data)
        |> ComponentData.getName


getHealth : Entity -> World -> Maybe Float
getHealth entity world =
    Ecs.World.enabledEntityComponents world entity
        |> List.map (\c -> c.data)
        |> ComponentData.getHealth


getStatusEffects : Entity -> World -> List StatusEffectData
getStatusEffects entity world =
    Ecs.World.enabledEntityComponents world entity
        |> List.map (\c -> c.data)
        |> List.filterMap
            (\a ->
                case a of
                    StatusEffect eff ->
                        Just eff

                    _ ->
                        Nothing
            )


getSkills : Entity -> World -> List SkillData
getSkills entity world =
    Ecs.World.enabledEntityComponents world entity
        |> List.map (\c -> c.data)
        |> List.filterMap
            (\a ->
                case a of
                    Skill eff ->
                        Just eff

                    _ ->
                        Nothing
            )


getSkillComponents : Entity -> World -> List Component
getSkillComponents entity world =
    Ecs.World.enabledEntityComponents world entity
        -- |> List.map (\c -> c.data)
        |> List.filterMap
            (\a ->
                case a.data of
                    Skill eff ->
                        Just a

                    _ ->
                        Nothing
            )


renderSkillComponents : List Component -> List (Html GameMsg)
renderSkillComponents components =
    let
        addIfNeeded : Bool -> Html.Attribute msg -> List (Html.Attribute msg)
        addIfNeeded isNeed attr =
            if isNeed then
                [ attr ]

            else
                []

        renderSkill : Component -> SkillData -> Html GameMsg
        renderSkill component skill =
            li
                (List.concat
                    [ [ class "skill" ]
                    , addIfNeeded skill.autoUse (class "active")
                    ]
                )
                [ button [ onClick (ToggleSkill component) ]
                    [ strong [] [ text skill.name ]
                    , br [] []
                    , text skill.description
                    , br [] []
                    , meter
                        [ step "any"
                        , value (String.fromFloat skill.energy)
                        , Html.Attributes.max (String.fromFloat skill.energyUse)
                        ]
                        []

                    -- , br [] []
                    -- , text ("auto: " ++ Debug.toString skill.autoUse)
                    , br [] []
                    , text ("trgt: " ++ Debug.toString skill.target)
                    ]
                ]
    in
    components
        |> List.filterMap
            (\c ->
                case c.data of
                    Skill skill ->
                        Just (renderSkill c skill)

                    _ ->
                        Nothing
            )


renderSkills : List SkillData -> List (Html msg)
renderSkills skills =
    List.map
        (\skill ->
            li [ class "skill" ]
                [ button []
                    [ strong [] [ text skill.name ]

                    -- , br [] []
                    -- , aside [ class "skill-description" ] [ text (String.fromFloat skill.energy) ]
                    , br [] []

                    -- , text ("energy: " ++ String.fromFloat skill.energy ++ " / " ++ String.fromFloat skill.energyUse)
                    , meter
                        [ step "any"
                        , value (String.fromFloat skill.energy)
                        , Html.Attributes.max (String.fromFloat skill.energyUse)
                        ]
                        []
                    , br [] []
                    , text ("auto: " ++ Debug.toString skill.autoUse)
                    , br [] []
                    , text ("target: " ++ Debug.toString skill.target)
                    ]
                ]
        )
        skills


renderStatusEffects : List StatusEffectData -> List (Html msg)
renderStatusEffects statusEffects =
    List.map
        (\eff ->
            li [ class "item" ]
                [ strong [] [ text eff.name ]
                , br [] []
                , aside [ class "item-description" ] [ text eff.description ]
                ]
        )
        statusEffects


renderEntity : World -> Entity -> Html GameMsg
renderEntity world entity =
    div [ class "game-entity" ]
        [ h1 [ class "entity-name" ]
            (List.filterMap
                (\name ->
                    case name of
                        Just n ->
                            Just (text n)

                        _ ->
                            Nothing
                )
                [ getName entity world ]
            )
        , div [ class "entity-stats" ]
            [ div [ class "entity-health" ]
                (List.filterMap
                    (\( label, val ) ->
                        case val of
                            Just v ->
                                Just
                                    (meter [ value (String.fromFloat v) ] [])

                            _ ->
                                Nothing
                    )
                    [ ( "Health", getHealth entity world )
                    ]
                )
            , ul [ class "skills" ] (getSkillComponents entity world |> renderSkillComponents)
            , ul [ class "items" ] (getStatusEffects entity world |> renderStatusEffects)
            , div []
                (List.filterMap
                    (\( label, val ) ->
                        case val of
                            Just v ->
                                Just (p [] [ text (label ++ ": " ++ String.fromFloat v) ])

                            _ ->
                                Nothing
                    )
                    [ ( "Damage", getStat entity world Stat.Damage )
                    , ( "Health", getStat entity world Stat.Health )
                    , ( "Shield", getStat entity world Stat.Shield )
                    , ( "Energy Regen", getStat entity world Stat.EnergyRegen )
                    ]
                )
            ]
        ]


render : World -> Html GameMsg
render world =
    section [ id "game" ]
        [ div [ id "players" ]
            (List.map
                (renderEntity world)
                (List.filter (Ecs.World.hasComponentData ComponentData.newPlayerComponentData world) world.entities)
                |> List.reverse
            )
        , div [ id "enemies" ]
            (List.map
                (renderEntity world)
                (List.filter (\e -> Ecs.World.hasComponentData ComponentData.newPlayerComponentData world e |> not) world.entities)
                |> List.reverse
            )
        ]
