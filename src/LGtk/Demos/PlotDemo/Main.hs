{-# LANGUAGE NoMonomorphismRestriction #-}
module LGtk.Demos.PlotDemo.Main
    ( main
    ) where

import Control.Monad

import Diagrams.Prelude hiding (vcat, hcat)
import Diagrams.TwoD.Shapes (hrule, vrule)

import LGtk
import LGtk.Demos.PlotDemo.ArithParser (parseArith)

main :: IO ()
main = runWidget mainWidget

mainWidget = notebook
    [ (,) "PDani" $ notebook

        [ (,) "Function" $ do
            equation <- newRef "x ^ 2 - 5"
            rangeXmin <- newRef "-10"
            rangeXmax <- newRef "10"
            rangeYmin <- newRef "-10"
            rangeYmax <- newRef "10"
            hcat
                [ canvas 200 200 20 (const $ return ()) Nothing (readRef equation) $
                    \str -> (snd . drawPlot) str # value () # lw 0.05
                , vcat
                    [ entry equation
                    , label $ liftM (((++) "Parse error: ") . fst . drawPlot) $ readRef equation
                    , hcat
                        [ label $ return "X axis range: "
                        , entry rangeXmin
                        , entry rangeXmax
                        ]
                    , hcat
                        [ label $ return "Y axis range: "
                        , entry rangeYmin
                        , entry rangeYmax
                        ]
                    ]
                ]
        ]
    ]

drawPlot :: String -> (String, Dia Any)
drawPlot str = either (flip (,) emptyPlot) ((,) "" . (flip (<>) emptyPlot)) res
  where res = drawFunc $ parseFunc str
  
emptyPlot :: Dia Any
emptyPlot = hrule 20.0 <> vrule 20.0

drawFunc :: (Double -> Either String Double) -> Either String (Dia Any)
drawFunc f = do
    let xs = [-10.0,-9.9..10.0]
    ys <- mapM f xs
    let pts = map p2 $ zip xs ys
    return $ fromVertices pts # lc blue

parseFunc :: String -> Double -> Either String Double
parseFunc str x = parseArith symTab str
  where symTab = [("pi", pi), ("e", exp 1.0), ("x", x)]
