module StatusEffect exposing
    ( StatusEffectData
    , StatusEffectDuration(..)
    , effectPresets
    , getStatOfType
    , newDuration
    , newStatusEffect
    , newUnlimitedStatusEffect
    , reduceDuration
    )

import Stat exposing (StatModifier, StatType, newStatModifier)


type StatusEffectDuration
    = Remaining Float
    | Unlimited


type alias StatusEffectData =
    { name : String
    , description : String
    , effects : List StatModifier
    , duration : StatusEffectDuration
    }


newStatusEffect : String -> String -> List StatModifier -> StatusEffectDuration -> StatusEffectData
newStatusEffect name description statModifiers duration =
    StatusEffectData name description statModifiers duration


newUnlimitedStatusEffect : String -> String -> List StatModifier -> StatusEffectData
newUnlimitedStatusEffect name description statModifiers =
    StatusEffectData name description statModifiers Unlimited


newDuration : Float -> StatusEffectDuration
newDuration duration =
    Remaining duration


reduceDuration : StatusEffectData -> Float -> StatusEffectData
reduceDuration effect value =
    case effect.duration of
        Remaining remaining ->
            { effect
                | duration = Remaining (max 0 (remaining - value))
            }

        Unlimited ->
            effect


effectPresets : List StatusEffectData
effectPresets =
    [ newStatusEffect
        "Glass"
        "More dmg, less hp"
        [ newStatModifier Stat.Damage Stat.Multiplicative 2
        , newStatModifier Stat.Health Stat.Multiplicative 0.5
        ]
        Unlimited
    , newStatusEffect
        "Reactor"
        "Energy regen"
        [ newStatModifier Stat.EnergyRegen Stat.Additive 1.1
        ]
        Unlimited
    , newStatusEffect
        "CI"
        "Lots of shield, 1 hp"
        [ newStatModifier Stat.Shield Stat.Multiplicative 3
        , newStatModifier Stat.Health Stat.Set 1
        ]
        Unlimited
    , newStatusEffect
        "Sword"
        "basic dmg"
        [ newStatModifier Stat.Damage Stat.Additive 5
        ]
        Unlimited
    , newStatusEffect
        "Heart"
        "basic health"
        [ newStatModifier Stat.Health Stat.Additive 5
        ]
        Unlimited
    , newStatusEffect
        "Stun"
        "no energy regen"
        [ newStatModifier Stat.EnergyRegen Stat.Set 0
        ]
        Unlimited
    , newStatusEffect
        "Shield"
        "basic shield"
        [ newStatModifier Stat.Shield Stat.Additive 5
        ]
        Unlimited
    ]


getStatOfType : List StatusEffectData -> StatType -> Maybe Float
getStatOfType effects statType =
    effects
        |> List.map (\effect -> effect.effects)
        |> List.foldl (++) []
        |> List.filter (\statModifier -> statModifier.statType == statType)
        |> Stat.sumModifiers
