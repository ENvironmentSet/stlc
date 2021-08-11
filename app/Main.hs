module Main where

import Text.Megaparsec
import Parser
import TypeChecker
import Transformer

main :: IO ()
main =  print $ do 
  ast <- parseMaybe stlc "(λ#a->a. 1) (λ#a. 1)"
  
  if typeCheck ast then eval ast else Nothing
