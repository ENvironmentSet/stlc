module Transformer where

import Parser

import Data.Maybe

import Find

type Env = [STLC]

beta :: STLC -> Maybe STLC -- this is not a beta reduction. name should be changed.
beta = beta' []
  where
    beta' :: Env -> STLC -> Maybe STLC
    beta' env (Var i) = find env (i - 1)
    beta' env (Abs _ body) = beta' env body
    beta' env (App head arg) = beta' (arg : env) head

isHNF :: STLC -> Bool
isHNF (App _ _) = False
isHNF _ = True

eval :: STLC -> Maybe STLC
eval ast | isHNF ast = Just ast
         | otherwise = beta ast >>= eval