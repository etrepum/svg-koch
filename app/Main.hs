module Main where

import Diagrams.Prelude (Diagram, strokeLoop)
import Diagrams.Backend.SVG.CmdLine (B, mainWith)
import Lib (koch)

example :: Diagram B
example = strokeLoop (koch 4)

main :: IO ()
main = mainWith example