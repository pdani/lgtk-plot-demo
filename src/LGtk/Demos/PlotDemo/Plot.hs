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

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

tr :: ViewPort -> (Double, Double)
tr = mapTuple $ (/ 2.0) . negate . uncurry (+)

sc :: ViewPort -> (Double, Double)
sc = mapTuple $ (1.0 /) . uncurry (flip (-))

defViewPort :: ViewPort
defViewPort = ((-10, 10), (-10, 10))

data PlotState = PlotState
    { _equation :: String
    , _viewport :: ViewPort
    }
  deriving (Eq)

$(makeLenses ''PlotState)

defPlotState :: PlotState
defPlotState = PlotState "x ^ 2 - 5" defViewPort

funcResolution :: Double
funcResolution = 1000

drawPlot :: Double -> PlotState -> (String, Dia ())
drawPlot lWidth (PlotState eq vp) = maybe ("Parse error", mempty) ((,) "" . (lineSet . transPlot . (flip (<>) $ emptyPlot vp))) res
  where res = drawFunc (fst vp) $ parseFunc eq
        transPlot d = d # translate (r2 $ tr vp) # scaleX ((fst . sc) vp) # scaleY ((snd . sc) vp)
        lineSet d = d # value () # lwL lWidth
        
emptyPlot :: ViewPort -> Dia Any
emptyPlot vp = hrule xsize <> vrule ysize
  where (xsize, ysize) = mapTuple ((* 2.1) . uncurry max . mapTuple abs) vp

drawFunc :: (Double, Double) -> (Double -> Maybe Double) -> Maybe (Dia Any)
drawFunc (xmin, xmax) f = do
    let xs = [xmin,xmin + step..xmax]
    ys <- mapM f xs
    let pts = map p2 $ zip xs ys
    return $ fromVertices pts # lc blue
  where step = (xmax - xmin) / funcResolution
