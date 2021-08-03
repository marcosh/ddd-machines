{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module App where

import Door

-- machines
import Data.Machine.Process
import Data.Machine.Type

appAggregateAndPolicy :: [DoorCommand] -> [(DoorState, DoorEvent)]
appAggregateAndPolicy commands = run $ supply commands doorAggregateAndPolicy

-- app :: [DoorCommand] -> [DoorOpenedCounter]
-- app commands = run $ supply commands doorProcess
