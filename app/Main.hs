{-# LANGUAGE TypeApplications #-}

module Main where

import App ( app )
import Door ( DoorCommand(Open) )

-- base
import Data.Functor.Identity ( Identity )

main :: IO ()
main = print $ app @Identity [Open, Open]
