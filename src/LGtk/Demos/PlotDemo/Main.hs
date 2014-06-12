{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
module LGtk.Demos.PlotDemo.Main
    ( main
    ) where

import Control.Monad
import Control.Lens hiding ((#))
import Debug.Trace
import Diagrams.Prelude hiding (vcat, hcat, interval, view, trace)
import Diagrams.TwoD.Shapes (hrule, vrule)

import LGtk
import LGtk.Demos.PlotDemo.ArithParser (parseArith)

type ViewPort = ((Double, Double), (Double, Double))

data PlotState = PlotState
    { _equation :: String
    , _viewport :: ViewPort
    }
  deriving (Eq)

$(makeLenses ''PlotState)

main :: IO ()
main = runWidget mainWidget

defViewPort :: ViewPort
defViewPort = ((-0.5, 0.5), (-0.5, 0.5))

mainWidget = notebook
    [ (,) "PDani" $ notebook

        [ (,) "Function" $ do
            plotState <- newRef $ PlotState "x ^ 2 - 5" defViewPort
            let (xmin, xmax) = interval $ viewport . _1 `lensMap` plotState
            let (ymin, ymax) = interval $ viewport . _2 `lensMap` plotState
            let errormsg = lens (fst . drawPlot) (const) `lensMap` plotState
            hcat
                [ canvas 600 600 1 (const $ return ()) Nothing (readRef plotState) $
                    \ps -> (snd . drawPlot) ps # value () # lw (1.0 / 600.0)
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

drawPlot :: PlotState -> (String, Dia Any)
drawPlot (PlotState eq vp) = maybe ("Parse error", emptyPlot defViewPort) ((,) "" . (transPlot . (flip (<>) $ emptyPlot vp))) res
  where ((xmin, xmax), (ymin, ymax)) = vp
        (xr, yr) = (xmax - xmin, ymax - ymin)
        (xa, ya) = ((xmax + xmin) / 2.0, (ymax + ymin) / 2.0)
        res = drawFunc vp $ parseFunc eq
        transPlot = flip (#) $ translate (r2 (-xa / 2.0, -ya / 2.0)) # scaleX (1.0 / xr) # scaleY (1.0 / yr)
        
emptyPlot :: ViewPort -> Dia Any
emptyPlot vp = hrule xsize <> vrule ysize
  where (xsize, ysize) = mapTuple ((* 2.1) . uncurry max . mapTuple abs) vp
        mapTuple f (a, b) = (f a, f b)

drawFunc :: ViewPort -> (Double -> Maybe Double) -> Maybe (Dia Any)
drawFunc ((xmin, xmax), (ymin, ymax)) f = do
    let xs = [xmin,xmin + (xmax - xmin) / 100.0..xmax]
    ys <- mapM f xs
    let pts = map p2 $ zip xs ys
    return $  fromVertices pts # lc blue

parseFunc :: String -> Double -> Maybe Double
parseFunc str x = either (const Nothing) Just $ parseArith symTab str
  where symTab = [("pi", pi), ("e", exp 1.0), ("x", x)]
