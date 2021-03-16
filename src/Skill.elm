module Skill exposing
    ( SkillData
    , SkillEffect(..)
    , newSkill
    , skillPresets
    )

import Ecs.Entity exposing (Entity)
import Stat
import StatusEffect exposing (StatusEffectData)


type SkillEffect
    = Damage Float
    | StatusEffect StatusEffectData


type alias SkillData =
    { name : String
    , description : String
    , autoUse : Bool
    , target : Maybe Entity
    , energyUse : Float
    , energy : Float
    , effects : List SkillEffect
    }


newSkill : String -> String -> Float -> List SkillEffect -> SkillData
newSkill name description energyUse effects =
    SkillData name description True Nothing energyUse 0 effects


skillPresets : List SkillData
skillPresets =
    [ newSkill "Laser" "basic attack" 20 [ Damage 0.5 ]
    , newSkill "Stun"
        "stun target"
        20
        [ StatusEffect
            (StatusEffect.newStatusEffect
                "Stun"
                "No energy regen"
                [ Stat.newStatModifier Stat.EnergyRegen Stat.Set 0 ]
                (StatusEffect.newDuration 1000)
            )
        ]
    ]
