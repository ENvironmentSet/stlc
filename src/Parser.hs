{-# LANGUAGE GADTs #-}

module Parser where

import Control.Monad
import Data.Void
import Data.Foldable

import Text.Megaparsec
import Text.Megaparsec.Char

data TY = TVar String | TLam TY TY deriving Eq
data STLC = Var Int | Abs TY STLC | App STLC STLC

instance Show TY where
  show (TVar x) = x
  show (TLam head body) = "(" ++ show head ++ ")->" ++ show body

instance Show STLC where
  show (Var i) = show i
  show (Abs t body) = "(λ#" ++ show t ++ ". " ++ show body ++ ")"
  show (App head arg) = "(" ++ show head ++ " " ++ show arg ++ ")"

type Parser = Parsec Void String

tvar :: Parser TY
tvar = fmap TVar $ (:) <$> letterChar <*> many alphaNumChar

tgrouping :: Parser TY
tgrouping = between (char '(' >> space) (space >> char ')') ty

tlam :: Parser TY
tlam = do
  arg <- tvar <|> tgrouping
  space
  string "->"
  space
  ret <- ty

  return $ TLam arg ret

ty :: Parser TY
ty = try tlam <|> tvar <|> tgrouping

variable :: Parser STLC
variable = Var . read <$> some numberChar

abstraction :: Parser STLC 
abstraction = do
  char 'λ'
  space
  char '#'
  t <- ty
  char '.' -- remove? only for readability.
  space
  body <- term

  return (Abs t body)

grouping :: Parser STLC
grouping = between (char '(' >> space) (space >> char ')') term

application :: Parser STLC
application = do
  head <- term'
  space
  args <- sepBy (term' <|> abstraction) space -- rises error when application is followed by spaces. should be fixed.
  
  return $ foldl' App head args

  where
    term' = grouping <|> variable

term :: Parser STLC
term = abstraction <|> application <|> grouping <|> variable

stlc :: Parser STLC
stlc = between space space term