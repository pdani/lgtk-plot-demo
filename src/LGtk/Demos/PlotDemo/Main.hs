{-# LANGUAGE NoMonomorphismRestriction #-}
module LGtk.Demos.PlotDemo.Main
    ( main
    ) where

import Control.Monad
import Control.Lens hiding ((#))

import LGtk
import LGtk.Demos.PlotDemo.Plot

canvasSize = 600
lWidth = 1 / fromIntegral canvasSize

main :: IO ()
main = runWidget mainWidget

mainWidget = notebook
    [ (,) "PDani" $ notebook

        [ (,) "Function" $ do
            plotState <- newRef defPlotState
            let (xmin, xmax) = interval $ viewport . _1 `lensMap` plotState
            let (ymin, ymax) = interval $ viewport . _2 `lensMap` plotState
            let errormsg = lens (fst . drawPlot lWidth) const `lensMap` plotState
            hcat
                [ canvas canvasSize canvasSize 1 (const $ return ()) Nothing (readRef plotState) $
                    \ps -> (snd . drawPlot lWidth) ps
                , vcat
                    [ entry $ equation `lensMap` plotState
                    , label $ readRef errormsg
                    , hcat
                        [ label $ return "X axis range: "
                        , entryShow xmin
                        , entryShow xmax
                        ]
                    , hcat
                        [ label $ return "Y axis range: "
                        , entryShow ymin
                        , entryShow ymax
                        ]
                    ]
                ]
        ]
    ]

interval :: (RefClass r, Ord a) => RefSimple r (a, a) -> (RefSimple r a, RefSimple r a)
interval ab = (lens fst set1 `lensMap` ab, lens snd set2 `lensMap` ab) where
    set1 (_, b) a = (min b a, b)
    set2 (a, _) b = (a, max a b)
