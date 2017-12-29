module Lib
    ( koch
    ) where

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
