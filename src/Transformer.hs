module Transformer where

import Parser

import Data.Maybe

import Find

beta :: STLC -> STLC
beta (App head arg) = unwrap $ beta' (eval arg) 0 (eval head)
  where
    beta' ::  STLC -> Int -> STLC -> STLC
    beta' arg index ast@(Var i) | i == index = arg
                                | otherwise = ast
    beta' arg index (Abs t body) = Abs t (beta' arg (index + 1) (eval body))
    beta' arg index (App head arg') =  App (beta' arg index (eval head)) (beta' arg index (eval arg'))
    unwrap :: STLC -> STLC
    unwrap (Abs _ body) = body
    unwrap ast = ast
beta ast = ast

isNF :: STLC -> Bool
isNF (App _ _) = False
isNF _ = True

eval :: STLC -> STLC -- result of evaluation must be Lambda
eval ast | isNF ast = ast
         | otherwise = eval $ beta ast