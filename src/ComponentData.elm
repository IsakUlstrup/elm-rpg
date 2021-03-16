module ComponentData exposing
    ( ComponentData(..)
    , componentTypeString
    , defaultData
    , editComponentData
    , getHealth
    , getName
    , isPlayerComponent
    , newHealthComponentData
    , newPlayerComponentData
    , newStatusEffectComponentData
    , toggleSkill
    )

import Html
import Html.Attributes
import Maybe
import Skill exposing (SkillData, SkillEffect(..))
import Stat exposing (StatModifier)
import StatusEffect exposing (StatusEffectData)



-- Extend this type to add new components


type ComponentData
    = Player
    | Name String
    | StatusEffect StatusEffectData
    | Skill SkillData
    | Health HealthData


type alias HealthData =
    Float



-- Helper functions


componentTypeString : ComponentData -> String
componentTypeString data =
    case data of
        -- Weapon _ ->
        --     "Weapon"
        Player ->
            "Player"

        Name _ ->
            "Name"

        StatusEffect _ ->
            "Status Effect"

        Skill _ ->
            "Skill"

        Health _ ->
            "Health"


type alias EditData msg =
    { label : String
    , element : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
    , attributes : List (Html.Attribute msg)
    , children : List (Html.Html msg)
    , update : String -> ComponentData
    }


renderStatusEffect : List StatModifier -> List (Html.Html msg)
renderStatusEffect modifiers =
    modifiers
        |> List.map
            (\mod ->
                Html.p []
                    [ Html.text (String.fromFloat mod.value)
                    , Html.text " "
                    , Html.text (Debug.toString mod.modifierType)
                    , Html.text " "
                    , Html.text (Debug.toString mod.statType)
                    ]
            )


editComponentData : ComponentData -> List (EditData msg)
editComponentData data =
    case data of
        Player ->
            []

        Name name ->
            [ { label = "Name"
              , element = Html.input
              , attributes =
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.value name
                    ]
              , children = []
              , update = \input -> Name input
              }
            ]

        StatusEffect effect ->
            let
                selectPreset stringName =
                    List.filter (\preset -> preset.name == stringName) StatusEffect.effectPresets
                        |> List.head
            in
            [ { label = "Effect"
              , element = Html.select
              , attributes =
                    []
              , children =
                    List.map
                        (\statusEffect ->
                            Html.option
                                [ Html.Attributes.selected (effect.name == statusEffect.name) ]
                                [ Html.text statusEffect.name ]
                        )
                        StatusEffect.effectPresets
              , update = \name -> StatusEffect (selectPreset name |> Maybe.withDefault effect)
              }

            -- , { label = "Name"
            --   , element = Html.input
            --   , attributes =
            --         [ Html.Attributes.type_ "text"
            --         , Html.Attributes.value effect.name
            --         ]
            --   , children = []
            --   , update = \input -> StatusEffect { effect | name = input }
            --   }
            , { label = "Description"
              , element = Html.p
              , attributes = []
              , children = [ Html.text effect.description ]
              , update = \_ -> StatusEffect effect
              }
            , { label = "Stat modifiers"
              , element = Html.p
              , attributes =
                    []
              , children = renderStatusEffect effect.effects
              , update = \_ -> StatusEffect effect
              }
            ]

        Skill skill ->
            let
                selectPreset stringName =
                    List.filter (\preset -> preset.name == stringName) Skill.skillPresets
                        |> List.head
            in
            [ { label = "Name"
              , element = Html.select
              , attributes =
                    []
              , children =
                    List.map
                        (\s ->
                            Html.option
                                [ Html.Attributes.selected (skill.name == s.name) ]
                                [ Html.text s.name ]
                        )
                        Skill.skillPresets
              , update = \name -> Skill (selectPreset name |> Maybe.withDefault skill)
              }
            , { label = "Description"
              , element = Html.p
              , attributes = []
              , children = [ Html.text skill.description ]
              , update = \_ -> Skill skill
              }
            , { label = "Effects"
              , element = Html.p
              , attributes =
                    []
              , children = List.map (\eff -> Html.p [] [ Html.text (Debug.toString eff) ]) skill.effects
              , update = \_ -> Skill skill
              }
            ]

        Health health ->
            [ { label = "Health"
              , element = Html.input
              , attributes =
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.step "any"
                    , Html.Attributes.max "1"
                    , Html.Attributes.value (String.fromFloat health)
                    ]
              , children = []
              , update = \newHealth -> Health (String.toFloat newHealth |> Maybe.withDefault 0)
              }
            ]


defaultData : List ( String, ComponentData )
defaultData =
    [ ( "Player", newPlayerComponentData )
    , ( "Name", Name "new name" )
    , ( "Status Effect"
      , StatusEffect
            (List.head StatusEffect.effectPresets
                |> Maybe.withDefault (StatusEffect.newUnlimitedStatusEffect "test item" "a cool item" [])
            )
      )
    , ( "Skill"
      , Skill
            (List.head Skill.skillPresets
                |> Maybe.withDefault (Skill.newSkill "test skill" "a skill" 30 [])
            )
      )
    , ( "Health", newHealthComponentData )
    ]


toggleSkill : ComponentData -> ComponentData
toggleSkill data =
    case data of
        Skill skill ->
            Skill { skill | autoUse = not skill.autoUse }

        _ ->
            data


newPlayerComponentData : ComponentData
newPlayerComponentData =
    Player


newHealthComponentData : ComponentData
newHealthComponentData =
    Health 1


newStatusEffectComponentData : StatusEffectData -> ComponentData
newStatusEffectComponentData data =
    StatusEffect data


isPlayerComponent : ComponentData -> Bool
isPlayerComponent data =
    data == Player


getName : List ComponentData -> Maybe String
getName components =
    let
        names =
            components
                |> List.filterMap
                    (\c ->
                        case c of
                            Name n ->
                                Just n

                            _ ->
                                Nothing
                    )
    in
    if List.length names /= 0 then
        Just (List.foldl (++) "" names)

    else
        Nothing


getHealth : List ComponentData -> Maybe Float
getHealth components =
    let
        healthComponents =
            components
                |> List.filterMap
                    (\a ->
                        case a of
                            Health health ->
                                Just health

                            _ ->
                                Nothing
                    )
    in
    if List.length healthComponents > 0 then
        Just (List.sum healthComponents / toFloat (List.length healthComponents))

    else
        Nothing
