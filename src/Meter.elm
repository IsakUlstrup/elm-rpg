module Meter exposing (Meter, add, filledPercentage, isEmpty, newEmpty, newFull, reset, subtract)


type alias Meter =
    { current : Int
    , max : Int
    , headingDown : Bool
    }


newFull : Int -> Meter
newFull max =
    Meter max max True


newEmpty : Int -> Meter
newEmpty max =
    Meter 0 max False


subtract : Int -> Meter -> Meter
subtract amount meter =
    { meter | current = meter.current - amount |> max 0, headingDown = True }


add : Int -> Meter -> Meter
add amount meter =
    { meter | current = meter.current + amount |> min meter.max, headingDown = False }


reset : Meter -> Meter
reset meter =
    { meter | current = 0 }


isEmpty : Meter -> Bool
isEmpty meter =
    meter.current == 0


filledPercentage : Meter -> Int
filledPercentage meter =
    (toFloat meter.current / toFloat meter.max * 100)
        |> round
