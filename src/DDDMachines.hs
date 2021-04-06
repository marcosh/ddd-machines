{-# LANGUAGE GADTs #-}

module DDDMachines where

-- base
import Control.Arrow ( Arrow((&&&)) )
import Data.Functor.Identity ( Identity(Identity) )

-- machines
import Data.Machine.Mealy ( unfoldMealy, Mealy )
import Data.Machine.Moore ( unfoldMoore, Moore )
import Data.Machine.MooreT ( unfoldMooreT, MooreT )
import Data.Machine.Process ( Automaton, AutomatonM )

data Aggregate automaton command event where
  Aggregate :: Automaton automaton => automaton command [event] -> Aggregate automaton command event

data Policy automaton monad event command where
  Policy :: (AutomatonM automaton, Monad monad) => automaton monad event command -> Policy automaton monad event command

data Projection automaton event state where
  Projection :: Automaton automaton => automaton event state -> Projection automaton event state

-- door

data DoorCommand = Open | Close

data DoorState = IsOpen | IsClosed

data DoorEvent = Opened | Closed

isOpened :: DoorEvent -> Bool
isOpened Opened = True
isOpened Closed = False

door :: Aggregate Mealy DoorCommand DoorEvent
door = Aggregate $ unfoldMealy action initialState
  where
    action :: DoorState -> DoorCommand -> ([DoorEvent], DoorState)
    action IsOpen   Open  = ([]      , IsOpen  )
    action IsOpen   Close = ([Closed], IsClosed)
    action IsClosed Open  = ([Opened], IsOpen  )
    action IsClosed Close = ([]      , IsClosed)

    initialState :: DoorState
    initialState = IsClosed

counter :: Projection Moore DoorEvent Int
counter = Projection $ unfoldMoore (id &&& count) initialCount
  where
    count :: Int -> DoorEvent -> Int
    count i Opened = i + 1
    count i Closed = i

    initialCount :: Int
    initialCount = 0

-- light

data LightCommand = TurnOn | TurnOff

data LightState = On | Off

data LightEvent = TurnedOn | TurnedOff

light :: Aggregate Mealy LightCommand LightEvent
light = Aggregate $ unfoldMealy action initialState
  where
    action :: LightState -> LightCommand -> ([LightEvent], LightState)
    action On  TurnOn  = ([]         , On )
    action On  TurnOff = ([TurnedOff], Off)
    action Off TurnOn  = ([TurnedOn ], On )
    action Off TurnOff = ([]         , Off)

    initialState :: LightState
    initialState = Off

state :: Projection Moore LightEvent String
state = Projection $ unfoldMoore (id &&& lightState) initialState
  where
    lightState :: String -> LightEvent -> String
    lightState _ TurnedOn  = "The light is on"
    lightState _ TurnedOff = "The light is off"

    initialState :: String
    initialState = ""

-- whenever the door is opened the light should turn on

-- this is pure, so we use the Identity monad
turnOnLightOnDoorOpened :: Policy MooreT Identity DoorEvent (Maybe LightCommand)
turnOnLightOnDoorOpened = Policy $ unfoldMooreT action initialState
  where
    action :: Bool -> Identity (Maybe LightCommand, DoorEvent -> Bool)
    action True  = Identity (Just TurnOn, isOpened)
    action False = Identity (Nothing    , isOpened)

    initialState :: Bool
    initialState = False