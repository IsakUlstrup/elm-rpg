module Content.Characters exposing (..)

import Character exposing (Character)
import Content.Skills
import Meter


ghost : Character
ghost =
    Character (Meter.newFull 50) [ Content.Skills.tackle ] "👻" 100 100 []


hero : Character
hero =
    Character (Meter.newFull 100) [ Content.Skills.tackle, Content.Skills.slam ] "🦸" 90 200 []
