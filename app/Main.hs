module Main where

import Text.Megaparsec
import Parser
import TypeChecker
import Transformer

main :: IO ()
main =  print $ do 
  ast <- parseMaybe stlc "(λ#(a->a)->(a->a). λ#a->a. 2 (2 1)) (λ#a->a. 1) (λ#a. 1)"
  
  if typeCheck ast then return $ eval ast else Nothing
