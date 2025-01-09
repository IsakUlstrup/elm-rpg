module Character exposing (Character, heal, hit, isDead, moveSkill, rollSkill)

import Meter exposing (Meter)
import Random exposing (Generator)
import Skill exposing (Skill)


type alias Character =
    { health : Meter
    , skills : List Skill
    , name : String
    , accuracy : Int
    , evasion : Int
    , healthHistory : List Int
    }


hit : Int -> Character -> Character
hit dmg character =
    { character
        | health = Meter.subtract dmg character.health
        , healthHistory = character.healthHistory ++ [ -dmg ]
    }


heal : Int -> Character -> Character
heal amount character =
    { character
        | health = Meter.add amount character.health
        , healthHistory = character.healthHistory ++ [ amount ]
    }


moveSkill : Character -> Character
moveSkill character =
    case character.skills of
        s :: ss ->
            { character | skills = ss ++ [ s ] }

        _ ->
            character


isDead : Character -> Bool
isDead character =
    character.health
        |> Meter.isEmpty


rollSkill : Skill -> Character -> Character -> Generator Int
rollSkill skill character target =
    let
        accuracyRatio : Float
        accuracyRatio =
            toFloat character.accuracy / toFloat target.evasion
    in
    Random.weighted ( accuracyRatio, True ) [ ( 1, False ) ]
        |> Random.andThen
            (\hitRoll ->
                if hitRoll then
                    Random.int skill.damageLow skill.damageHigh

                else
                    Random.constant 0
            )
