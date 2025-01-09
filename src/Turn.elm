module Turn exposing
    ( Turn
    , attacking
    , duration
    , isAttacking
    , isEnemyDoneAttacking
    , isEnemyDoneIdling
    , isEnemyTurn
    , isPlayerTurn
    , isTakingDamage
    , playerIdle
    , tick
    )

{-| State isPlayer cooldown
-}


type Turn
    = Attacking Bool Int
    | Idle Bool Int


duration : number
duration =
    150


attacking : Bool -> Turn
attacking isPlayer =
    Attacking isPlayer duration


playerIdle : Turn
playerIdle =
    Idle True duration


isPlayerTurn : Turn -> Bool
isPlayerTurn turn =
    case turn of
        Attacking True _ ->
            True

        Idle True _ ->
            True

        _ ->
            False


isEnemyTurn : Turn -> Bool
isEnemyTurn turn =
    case turn of
        Attacking False _ ->
            True

        Idle False _ ->
            True

        _ ->
            False


isTakingDamage : Bool -> Turn -> Bool
isTakingDamage isPlayer turn =
    case turn of
        Attacking ip _ ->
            ip /= isPlayer

        _ ->
            False


isAttacking : Bool -> Turn -> Bool
isAttacking isPlayer turn =
    case turn of
        Attacking ip _ ->
            ip == isPlayer

        _ ->
            False


isEnemyDoneIdling : Turn -> Bool
isEnemyDoneIdling turn =
    case turn of
        Idle False 0 ->
            True

        _ ->
            False


isEnemyDoneAttacking : Turn -> Bool
isEnemyDoneAttacking turn =
    case turn of
        Attacking False 0 ->
            True

        _ ->
            False


{-| tick turn with dt in ms
-}
tick : Int -> Turn -> Turn
tick dt turn =
    case turn of
        Attacking isPlayer 0 ->
            Idle (not isPlayer) 700

        Attacking isPlayer cd ->
            Attacking isPlayer (cd - dt |> max 0)

        Idle True _ ->
            turn

        Idle False cd ->
            Idle False (cd - dt |> max 0)
