{-# LANGUAGE NoMonomorphismRestriction #-}
module LGtk.Demos.PlotDemo.Main
    ( main
    ) where

import Diagrams.Prelude hiding (vcat, hcat)
import Diagrams.TwoD.Shapes (hrule, vrule)

import LGtk

import LGtk.Demos.PlotDemo.Lexer (tokenize)
import LGtk.Demos.PlotDemo.Parser (parse)
import LGtk.Demos.PlotDemo.Evaluator (evaluate)

import qualified Data.Map as M
import Control.Monad

main :: IO ()
main = runWidget mainWidget

mainWidget = notebook
    [ (,) "PDani" $ notebook

        [ (,) "Function" $ do
            equation <- newRef "x * x - 5"
            errorMsg <- newRef "No error"
            hcat
                [ canvas 200 200 20 (const $ return ()) Nothing (readRef equation) $
                    \x -> drawGraph (parseFunc x) # value () # lw 0.05
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

parseFunc str x = fst $ evaluate tree symTab
  where toks = tokenize str
        tree = parse toks
        symTab = M.fromList [("pi", pi), ("e", exp 1.0), ("x", x)]
