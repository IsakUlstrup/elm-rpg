module Stat exposing
    ( StatModifier
    , StatModifierType(..)
    , StatType(..)
    , getModifiersOfType
    , newStatModifier
    , sumModifiers
    )


type StatType
    = Health
    | Damage
    | EnergyRegen
    | Energy
    | Shield
    | ShieldRechargeCooldown
    | ShieldRechargeRate


type StatModifierType
    = Additive
    | Multiplicative
    | Set


type alias StatModifier =
    { statType : StatType
    , modifierType : StatModifierType
    , value : Float
    }


getModifiersOfType : List StatModifier -> StatType -> List StatModifier
getModifiersOfType modifiers statType =
    List.filter (\m -> m.statType == statType) modifiers


newStatModifier : StatType -> StatModifierType -> Float -> StatModifier
newStatModifier statType modifierType value =
    StatModifier statType modifierType value


sumModifiers : List StatModifier -> Maybe Float
sumModifiers modifiers =
    let
        modifiersByModType : List StatModifier -> StatModifierType -> List Float
        modifiersByModType mods modType =
            mods
                |> List.filter
                    (\c -> c.modifierType == modType)
                |> List.map (\a -> a.value)

        sum : StatModifierType -> Maybe Float
        sum modType =
            if List.length (modifiersByModType modifiers modType) /= 0 then
                Just
                    (modifiersByModType modifiers modType
                        |> List.foldl
                            (+)
                            0.0
                    )

            else
                Nothing
    in
    case sum Set of
        Just a ->
            Just (a / toFloat (List.length (modifiersByModType modifiers Set)))

        Nothing ->
            Maybe.map
                (\a ->
                    a
                        * (sum Multiplicative
                            |> Maybe.withDefault 1
                          )
                )
                (sum Additive)
