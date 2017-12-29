module Lib
    ( koch
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

koch :: Int -> Trail' Loop V2 Double
koch n = glueLine . mconcat . iterateN 3 (rotateBy (-1/3)) $ kochSegment n

kochSegment :: Int -> Trail' Line V2 Double
kochSegment n = case compare n 1 of
    LT -> mempty
    EQ -> kochLine
    GT -> seg
       ||| (seg # rotate (-45 @@ deg) # translateY ty)
       ||| (seg # rotate (45 @@ deg) # translateY ty)
       ||| seg
    where
        seg = kochSegment (n - 1) # scale t
        t = 1 / 3
        ty = t / 2

kochLine :: Trail' Line V2 Double
kochLine = fromOffsets . map r2 $ [(1, 0)]
