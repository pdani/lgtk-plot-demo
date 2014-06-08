module LGtk.Demos.PlotDemo.ArithParser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Expression
import Data.ByteString.Char8 hiding (map, empty)

binary  name fun assoc = Infix (fun <$ string (pack name)) assoc
prefix  name fun       = Prefix (fun <$ string (pack name))
postfix name fun       = Postfix (fun <$ string (pack name))

table = [ [ prefix "-" negate, prefix "+" id ]
        , [ prefix "abs" abs, prefix "sqrt" sqrt, prefix "log" log
          , prefix "sin" sin, prefix "tan" tan, prefix "cos" cos
          , prefix "asin" asin, prefix "atan" atan, prefix "acos" acos
          , prefix "sinh" sinh, prefix "tanh" tanh, prefix "cosh" cosh
          , prefix "asinh" asinh, prefix "atanh" atanh, prefix "acosh" acosh
          , prefix "trunc" (\x -> fromIntegral (truncate x :: Integer))
          , prefix "round" (\x -> fromIntegral (round x :: Integer))
          , prefix "ceil" (\x -> fromIntegral (ceiling x :: Integer))
          , prefix "floor" (\x -> fromIntegral (floor x :: Integer))
          ]
          
        , [ binary "^" (**) AssocRight ]
        
        , [ binary "*" (*) AssocLeft, binary "/" (/) AssocLeft ]
        
        , [ binary "+" (+) AssocLeft, binary "-" (-) AssocLeft ]
        ]

symbol :: [(ByteString, Double)] -> Parser Double
symbol symTab = choice $ map f symTab
  where f (sym, val) = string sym *> pure val

strip p = skipSpace *> p <* skipSpace

parens p = char '(' *> p <* char ')'

term symTab = strip p
  where p =  parens (expr symTab)
         <|> double
         <|> symbol symTab
         <?> "simple expression"

expr symTab = strip p
  where p =  buildExpressionParser table (term symTab)
         <?> "expression"

parseArith symTab str = parseOnly (expr $ map f symTab) $ pack str
  where f (s, v) = (pack s, v)