{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Data.Colour.Palette.ColorSet
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Lib (koch)

kochWidth :: Int -> Double
kochWidth n = 3 ^ (n - 1)

kochScale :: Int -> Double
kochScale n = (0.7 ^ (n - 2)) * (1 / kochWidth n)

kochLoop :: Int -> Diagram B
kochLoop n = lc (d3Colors1 n) . scale (kochScale n) . center . strokeLoop $ koch n

example :: Int -> Diagram B
example n = mconcat (map kochLoop [2..n])

main :: IO ()
main = mainWith example