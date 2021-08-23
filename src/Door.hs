module Door where

import DDD ( Aggregate(..), Policy(..), Projection(..) )
import Machines ( feedback )
import Mealy ( mealyT, run, runMealyT, stateless, unfoldMealy, MealyT )

data DoorCommand = Knock | Open | Close
  deriving (Eq, Show)

data DoorState = IsOpen | IsClosed
  deriving (Eq, Show)

data DoorEvent = Knocked | Opened | Closed
  deriving (Eq, Show)

door :: Aggregate DoorCommand DoorEvent
door = Aggregate $ unfoldMealy action initialState
  where
    action :: DoorState -> DoorCommand -> ([DoorEvent], DoorState)
    action IsOpen   Knock = ([Knocked], IsOpen  )
    action IsOpen   Open  = ([]       , IsOpen  )
    action IsOpen   Close = ([Closed] , IsClosed)
    action IsClosed Knock = ([Knocked], IsClosed)
    action IsClosed Open  = ([Opened] , IsOpen  )
    action IsClosed Close = ([]       , IsClosed)

    initialState :: DoorState
    initialState = IsClosed

type DoorOpenedCounter = Int

counter :: Projection DoorEvent DoorOpenedCounter
counter = Projection $ unfoldMealy action initialState
  where
    action :: DoorOpenedCounter -> DoorEvent -> (DoorOpenedCounter, DoorOpenedCounter)
    action i Opened = (i + 1, i + 1)
    action i _      = (i, i)

    initialState :: DoorOpenedCounter
    initialState = 0

type ShouldOpenDoor = Bool

shouldOpen :: DoorEvent -> ShouldOpenDoor
shouldOpen Knocked = True
shouldOpen _       = False

doorOpensOnKnock :: Monad m => Policy m DoorEvent DoorCommand
doorOpensOnKnock = Policy $ stateless shouldOpenOnKnocked
  where
    shouldOpenOnKnocked :: DoorEvent -> [DoorCommand]
    shouldOpenOnKnocked Knocked = [Open]
    shouldOpenOnKnocked _       = []

doorAggregateAndPolicy :: Monad m => MealyT m DoorCommand [DoorEvent]
doorAggregateAndPolicy =
  let
    Aggregate doorAggregate = door
    Policy doorPolicy = doorOpensOnKnock
  in
    feedback doorAggregate doorPolicy

doorProcess :: (Monad m, Semigroup p, Show p) => p -> MealyT m  DoorCommand [DoorEvent] -> MealyT m DoorEvent p -> MealyT m DoorCommand p
doorProcess initial aggregateAndPolicy projection =
  mealyT $ \c -> do
      (es, aggregateAndPolicy') <- runMealyT aggregateAndPolicy c

      (p, projection') <- run projection initial es

      pure (p, doorProcess p aggregateAndPolicy' projection')
