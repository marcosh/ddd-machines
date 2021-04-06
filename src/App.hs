{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module App where

import DDDMachines

-- machines
import Data.Machine.Process
import Data.Machine.Source
import Data.Machine.Type

-- data Command
--   = DoorCommand  DoorCommand
--   | LightCommand LightCommand

-- newtype State = State (DoorState, LightState)

app :: [DoorCommand] -> [DoorOpenedCounter]
app commands =
  let
    (Aggregate doorAutomaton) = door
    (Projection doorProjectionAutomaton) = counter
  in
    run $
         source commands
      ~> auto doorAutomaton
      ~> flattened
      ~> auto doorProjectionAutomaton
