{-|
Module      : RabitRelationParser
Description : Rabit relation parser
Author      : Vojtech Havlena, March 2019
License     : GPL-3
-}

module RabitRelationParser where

import Control.Applicative ((<*))

import Data.List

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Token
import qualified Data.Set as Set

import BuchiAutomaton
import qualified RabitAutomataParser as RP
import qualified AuxFunctions as Aux

--------------------------------------------------------------------------------------------------------------
-- Part with the data types
--------------------------------------------------------------------------------------------------------------

type RabitTuple = (RP.RabitState, String, RP.RabitState, String)
type RabitRelation = [RabitTuple]

--------------------------------------------------------------------------------------------------------------
-- Part with the definitions of lexemes
--------------------------------------------------------------------------------------------------------------

lexer = makeTokenParser emptyDef
m_state = lexeme lexer (many1 (noneOf " \r\n,->"))
m_string = lexeme lexer (many1 (noneOf " \r\n"))

parseFile :: FilePath -> IO RabitRelation
parseFile filename = do
  str <- readFile filename
  return $ parseString str


parseString :: String -> RabitRelation
parseString str = case (parse parseRabitRelation "RabitRelationParser" str) of
  Left err   -> error $ show err
  Right res  -> res


--------------------------------------------------------------------------------------------------------------
-- Part with the definitions of parsing rules
--------------------------------------------------------------------------------------------------------------


parseTuple :: Parser RabitTuple
parseTuple = do
  st1 <- m_state
  aut1 <- m_string
  st2 <- m_state
  aut2 <- m_string
  return (st1, aut1, st2, aut2)


parseRabitRelation :: Parser RabitRelation
parseRabitRelation = do
  trans <- many (try parseTuple)
  return trans
