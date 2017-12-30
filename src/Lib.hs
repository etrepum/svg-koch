module Lib
    ( kochExample
    ) where

import Data.Colour.Palette.ColorSet
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

koch :: Int -> Trail' Loop V2 Double
koch n = glueLine . mconcat . iterateN 3 (rotateBy (-1/3)) $ kochSegment n

kochSegment :: Int -> Trail' Line V2 Double
kochSegment n = case compare n 1 of
    LT -> mempty
    EQ -> fromOffsets [unitX]
    GT -> mconcat
        [ seg
        , seg # rotateBy (1/6)
        , seg # rotateBy (-1/6)
        , seg
        ]
    where
        seg = kochSegment (n - 1)

kochWidth :: Int -> Double
kochWidth n = 3 ^ (n - 1)

kochScale :: Int -> Double
kochScale n = (0.7 ^ (n - 2)) * (1 / kochWidth n)

kochLoop :: Int -> Diagram B
kochLoop n = lc (d3Colors1 n) . scale (kochScale n) . center . strokeLoop $ koch n

kochExample :: Int -> Diagram B
kochExample n = mconcat (map kochLoop [2..n])
