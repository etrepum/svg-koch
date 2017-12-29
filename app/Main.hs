module Main where

import Diagrams.Backend.SVG.CmdLine (mainWith)
import Lib (koch)

main :: IO ()
main = mainWith (koch 4)
