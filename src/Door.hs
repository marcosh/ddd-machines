{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Door where

-- import Machines (feedback)

-- base
import Control.Applicative (empty)
import Control.Arrow ( Arrow((&&&)) )
-- import System.IO.Unsafe

-- machines
import Data.Machine.Is ( Is )
import Data.Machine.Mealy ( unfoldMealy )
import Data.Machine.Moore ( unfoldMoore )
import Data.Machine.Plan ( PlanT, await, yield )
import Data.Machine.Process ( Automaton (auto), Process, ProcessT, (~>), flattened )
import Data.Machine.Type ( construct )

newtype Aggregate command event = Aggregate (Process command event)

data Policy monad event command where
  Policy :: Monad monad => ProcessT monad event command -> Policy monad event command

newtype Projection event state = Projection (Process event state)

-- door

data DoorCommand = Knock | Open | Close
  deriving (Eq, Show)

data DoorState = IsOpen | IsClosed
  deriving (Eq, Show)

data DoorEvent = Knocked | Opened | Closed
  deriving (Eq, Show)

door :: Aggregate DoorCommand (DoorState, DoorEvent)
door = Aggregate $ auto (unfoldMealy action initialState) ~> flattened
  where
    action :: DoorState -> DoorCommand -> ([(DoorState, DoorEvent)], DoorState)
    action IsOpen   Knock = ([(IsOpen, Knocked)], IsOpen  )
    action IsOpen   Open  = ([]       , IsOpen  )
    action IsOpen   Close = ([(IsClosed, Closed)] , IsClosed)
    action IsClosed Knock = ([(IsClosed, Knocked)], IsClosed)
    action IsClosed Open  = ([(IsOpen, Opened)] , IsOpen  )
    action IsClosed Close = ([]       , IsClosed)

    initialState :: DoorState
    initialState = IsClosed

    -- logState :: (DoorState -> DoorCommand -> ([DoorEvent], DoorState)) -> (DoorState -> DoorCommand -> ([DoorEvent], DoorState))
    -- logState action' initialState' command  = unsafePerformIO $ do
    --   let (events, finalState) = action' initialState' command
    --   print (initialState', command, events, finalState)
    --   pure (events, finalState)

type DoorOpenedCounter = Int

counter :: Projection DoorEvent DoorOpenedCounter
counter = Projection $ auto $ unfoldMoore (id &&& count) initialCount
  where
    count :: DoorOpenedCounter -> DoorEvent -> DoorOpenedCounter
    count i Opened  = i + 1
    count i _       = i

    initialCount :: DoorOpenedCounter
    initialCount = 0

type ShouldOpenDoor = Bool

shouldOpen :: DoorEvent -> ShouldOpenDoor
shouldOpen Knocked = True
shouldOpen _       = False

doorOpensOnKnock :: Monad m => Policy m (DoorState, DoorEvent) DoorCommand
doorOpensOnKnock = Policy $ construct shouldOpenOnKnocked
  where
    shouldOpenOnKnocked :: Monad m => PlanT (Is (DoorState, DoorEvent)) DoorCommand m ()
    shouldOpenOnKnocked = do
      event <- await
      case event of
        (_, Knocked) -> yield Open
        _       -> empty

doorAggregateAndPolicy :: Process DoorCommand (DoorState, DoorEvent)
doorAggregateAndPolicy = undefined
--   let
--     (Aggregate doorAggregateProcess) = door
--     (Policy doorPolicyProcess) = doorOpensOnKnock
--   in
--     feedback doorAggregateProcess doorPolicyProcess

-- doorProcess :: Process DoorCommand DoorOpenedCounter
-- doorProcess =
--   let
--     (Projection doorProjectionProcess) = counter
--   in
--     doorAggregateAndPolicy
--     ~> doorProjectionProcess
