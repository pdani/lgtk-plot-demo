{-# LANGUAGE TemplateHaskell #-}
module LGtk.Demos.PlotDemo.Plot
    ( defPlotState
    , viewport
    , equation
    , drawPlot
    ) where

import Control.Lens hiding ((#))

import Diagrams.Prelude hiding (trace)

import LGtk (Dia)

import LGtk.Demos.PlotDemo.ArithParser (parseFunc)

type ViewPort = ((Double, Double), (Double, Double))

tr :: ViewPort -> (Double, Double)
tr ((x1, x2), (y1, y2)) = (-(x1 + x2) / 2.0, -(y1 + y2) / 2.0)

sc :: ViewPort -> (Double, Double)
sc ((x1, x2), (y1, y2)) = (1.0 / (x2 - x1), 1.0 / (y2 - y1))

defViewPort :: ViewPort
defViewPort = ((-0.5, 0.5), (-0.5, 0.5))

data PlotState = PlotState
    { _equation :: String
    , _viewport :: ViewPort
    }
  deriving (Eq)

$(makeLenses ''PlotState)

defPlotState :: PlotState
defPlotState = PlotState "x ^ 2 - 5" defViewPort

drawPlot :: Double -> PlotState -> (String, Dia ())
drawPlot lWidth (PlotState eq vp) = maybe ("Parse error", (lineSet . emptyPlot) defViewPort) ((,) "" . (lineSet . transPlot . (flip (<>) $ emptyPlot vp))) res
  where res = drawFunc (fst vp) $ parseFunc eq
        transPlot d = d # translate (r2 $ tr vp) # scaleX ((fst . sc) vp) # scaleY ((snd . sc) vp)
        lineSet d = d # value () # lw lWidth
        
emptyPlot :: ViewPort -> Dia Any
emptyPlot vp = hrule xsize <> vrule ysize
  where (xsize, ysize) = mapTuple ((* 2.1) . uncurry max . mapTuple abs) vp
        mapTuple f (a, b) = (f a, f b)

drawFunc :: (Double, Double) -> (Double -> Maybe Double) -> Maybe (Dia Any)
drawFunc (xmin, xmax) f = do
    let xs = [xmin,xmin + (xmax - xmin) / 100.0..xmax]
    ys <- mapM f xs
    let pts = map p2 $ zip xs ys
    return $ fromVertices pts # lc blue
