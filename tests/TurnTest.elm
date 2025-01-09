module TurnTest exposing (constructor)

import Expect
import Test exposing (Test, describe, test)
import Turn


constructor : Test
constructor =
    describe "Turn constructors"
        [ test "Player turn constructor and isPlayerTurn predicate" <|
            \_ ->
                Turn.playerIdle
                    |> Turn.isPlayerTurn
                    |> Expect.equal True
        , test "Attacking turn constructor and isEnemyTurn predicate" <|
            \_ ->
                Turn.attacking False
                    |> Turn.isEnemyTurn
                    |> Expect.equal True
        ]



-- timing : Test
-- timing =
--     describe "Turn timing"
--         [ fuzz int "Fuzz tick attacking turn state" <|
--             \randomDt ->
--                 Turn.attacking False
--                     |> Turn.tick randomDt
--                     |> Turn.tick 0
--                     |> Turn.isTakingDamage True
--                     |> Expect.equal (randomDt > 1000)
--         , test "tick attacking turn state twice" <|
--             \_ ->
--                 Turn.attacking False
--                     |> Turn.tick 1000
--                     |> Turn.tick 0
--                     -- Player taking damage state
--                     |> Turn.tick 1000
--                     |> Turn.tick 0
--                     -- Player idle state
--                     |> Turn.isPlayerTurn
--                     |> Expect.equal True
--         ]
