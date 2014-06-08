{-# LANGUAGE NoMonomorphismRestriction #-}
module LGtk.Demos.PlotDemo.Main
    ( main
    ) where

import Control.Monad

import Diagrams.Prelude hiding (vcat, hcat)
import Diagrams.TwoD.Shapes (hrule, vrule)

import LGtk
import Data.ByteString.Char8 as BS hiding (map)
import LGtk.Demos.PlotDemo.ArithParser (parseArith)

main :: IO ()
main = runWidget mainWidget

mainWidget = notebook
    [ (,) "PDani" $ notebook

        [ (,) "Function" $ do
            equation <- newRef "x * x - 5"
            errorMsg <- newRef "No error"
            hcat
                [ canvas 200 200 20 (const $ return ()) Nothing (readRef equation) $
                    \str -> drawGraph (parseFunc str) # value () # lw 0.05
                , vcat
                    [ entry equation
                    , label $ readRef errorMsg
                    ]
                ]
        ]
    ]

drawGraph :: (Double -> Double) -> Dia Any
drawGraph f = hrule 20.0 <> vrule 20.0
           <> (fromVertices $ map p2 [(x, f x) | x <- [-10, -9.9..10]]) # lc blue

parseFunc str x = case res of Left _  -> 0.0
                              Right x -> x
  where symTab = [("pi", pi), ("e", exp 1.0), ("x", x)]
        res = parseArith symTab str
