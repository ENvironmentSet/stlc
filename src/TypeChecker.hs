module TypeChecker where

import Parser

import Control.Monad.State
import Data.Maybe
import Find

data TypedAST = TyVar Int TY | TyAbs TY TypedAST TY | TyApp TypedAST TypedAST TY deriving Show

type TypeEnv = [TY]

getType :: TypedAST -> TY
getType (TyVar _ t) = t
getType (TyAbs _ _ t) = t
getType (TyApp _ _ t) = t

tagType :: STLC -> Maybe TypedAST
tagType = tagType' []
  where
    tagType' :: TypeEnv -> STLC -> Maybe TypedAST
    tagType' env (Var i) = TyVar i <$> find env (i - 1)
    tagType' env (Abs pt body) = do 
      tbody <- tagType' (pt : env) body

      return $ TyAbs pt tbody (TLam pt (getType tbody))
    tagType' env (App head arg) = do
      thead <- tagType' env head
      targ <- tagType' env arg
      let theadType = getType thead

      case theadType of 
        TLam ht bt -> if ht == getType targ then Just $ TyApp thead targ bt else Nothing
        _ -> Nothing

typeCheck :: STLC -> Bool
typeCheck = isJust . tagType