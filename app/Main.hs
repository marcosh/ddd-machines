module Main where

import App
import DDDMachines

main :: IO ()
main = print $ app [Open, Open, Close, Open]
