{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  LGtk.Demos.PlotDemo.ArithParser
-- Copyright   :  (c) Daniel Pek 2014
-- License     :  see LICENSE
--
-- Maintainer  :  pekdaniel@gmail.com
--
-- Simple arithmetic parser using Attoparsec.
--
-----------------------------------------------------------------------------
module LGtk.Demos.PlotDemo.ArithParser
    ( parseFunc
    ) where

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

data Assoc = LeftAssoc | RightAssoc

data Op a
  = PrefixOps [Parser (a -> a)]
  | InfixOps Assoc [Parser (a -> a -> a)]

prefix :: BS.ByteString -> (a -> a) -> Parser (a -> a)
prefix str f = string str *> skipSpace *> return f

infix_ :: BS.ByteString -> (a -> a -> a) -> Parser (a -> a -> a)
infix_ str f = string str *> skipSpace *> return f

table :: [Op Double]
table
  = [ InfixOps LeftAssoc [ infix_ "-" (-), infix_ "+" (+) ]
    , InfixOps LeftAssoc [ infix_ "*" (*), infix_ "/" (/) ]
    , PrefixOps [ prefix "-" negate, prefix "+" id ]
    , InfixOps RightAssoc [infix_ "^" (**)]
    , PrefixOps [ prefix "abs" abs, prefix "sqrt" sqrt, prefix "log" log
                , prefix "sin" sin, prefix "tan" tan, prefix "cos" cos
                , prefix "asin" asin, prefix "atan" atan, prefix "acos" acos
                , prefix "sinh" sinh, prefix "tanh" tanh, prefix "cosh" cosh
                , prefix "asinh" asinh, prefix "atanh" atanh, prefix "acosh" acosh
                , prefix "trunc" (\x -> fromIntegral (truncate x :: Integer))
                , prefix "round" (\x -> fromIntegral (round x :: Integer))
                , prefix "ceil" (\x -> fromIntegral (ceiling x :: Integer))
                , prefix "floor" (\x -> fromIntegral (floor x :: Integer))
                ]

    ]

symbol :: [(String, Double)] -> Parser Double
symbol symTab = choice $ map f symTab
  where f (sym, val) = string (BS.pack sym) *> pure val

p :: [Op Double] -> [(String, Double)] -> Parser Double
p t s = skipSpace *> p' t

  where
    p' :: [Op Double] -> Parser Double
    p' (InfixOps LeftAssoc ps : ops) = do
      val <- p' ops
      as <- many (skipSpace *> liftA2 (,) (choice ps) (p' ops))
      return $ foldr (\(f, a) b -> f b a) val as

    p' (InfixOps RightAssoc ps : ops) = do
      val <- p' ops
      as <- many (skipSpace *> liftA2 (,) (choice ps) (p' ops))
      return $ foldr (\(f, b) g a -> f a (g b)) id as val

    p' (PrefixOps ps : ops) = do
      fs <- many (choice ps)
      val <- p' ops
      return $ foldl (flip ($)) val fs
    p' [] = do
      choice [ char '(' *> skipSpace *> p' t <* skipSpace <* char ')'
             , symbol s
             , double
             ] <* skipSpace

parseArith symTab = parseOnly (p table symTab <* endOfInput) . BS.pack

-- | 'parseFunc' returns a 'Maybe' 'Double', representing the value of the
-- expression, or a parser error. It accepts @expr@, a 'String' parameter
-- cotaining the string to be parsed, an @x@ value of 'Double', holding
-- the value of the symbol @x@ potentially used in the expression.
parseFunc :: String -> Double -> Maybe Double
parseFunc str x = either (const Nothing) Just $ parseArith symTab str
  where symTab = [("pi", pi), ("e", exp 1.0), ("x", x)]
