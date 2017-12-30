module Main where

import Diagrams.Backend.SVG.CmdLine (mainWith)
import Lib (kochExample)

main :: IO ()
main = mainWith kochExample