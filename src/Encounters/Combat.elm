module Encounters.Combat exposing (..)

import Character exposing (Character)
import Content.Characters
import Meter
import Turn exposing (Turn)


type alias Combat =
    { enemy : Character
    , turn : Turn
    }


type Msg
    = ClickedPlayerSkill Int


newCombat : Combat
newCombat =
    Combat Content.Characters.ghost Turn.playerIdle


tickModel : Float -> Combat -> Combat
tickModel dt model =
    { model | turn = Turn.tick (round dt) model.turn }



-- enemyAi : Combat -> Combat
-- enemyAi model =
--     if Turn.isEnemyDoneAttacking model.turn then
--         useEnemySkill model
--     else
--         model


advanceTurn : Combat -> Combat
advanceTurn model =
    if Turn.isEnemyDoneIdling model.turn && (Meter.isEmpty model.enemy.health |> not) then
        { model | turn = Turn.attacking False }

    else
        model



-- usePlayerSkill : Random.Seed -> Int -> ( Character, Combat ) -> ( Character, Combat )
-- usePlayerSkill seed index ( player, model ) =
--     case player.skills |> List.drop index |> List.head of
--         Just skill ->
--             let
--                 ( dmgRoll, _ ) =
--                     Random.step (rollSkill skill player model.enemy) seed
--             in
--             ( player
--             , { model
--                 | turn = Turn.attacking True
--                 , enemy = hit dmgRoll model.enemy
--               }
--             )
--         Nothing ->
--             ( player, model )
-- useEnemySkill : Random.Seed -> ( Character, Combat ) -> ( Character, Combat )
-- useEnemySkill seed ( player, model ) =
--     if Turn.isEnemyDoneAttacking model.turn then
--         case model.enemy.skills |> List.head of
--             Just skill ->
--                 let
--                     ( dmgRoll, _ ) =
--                         Random.step (Character.rollSkill skill model.enemy player) seed
--                 in
--                 ( Character.hit dmgRoll player
--                 , { model
--                     | enemy = Character.moveSkill model.enemy
--                   }
--                 )
--             Nothing ->
--                 ( player, model )
--     else
--         ( player, model )
