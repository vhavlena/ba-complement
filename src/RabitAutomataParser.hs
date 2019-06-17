{-|
  Module      : RabitAutomataParser
  Description : Rabit automata parser
  Author      : Vojtech Havlena, February 2019
  License     : GPL-3
-}

module RabitAutomataParser where

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
import qualified Data.Map as Map

import BuchiAutomaton
import qualified AuxFunctions as Aux

--------------------------------------------------------------------------------------------------------------
-- Part with the data types
--------------------------------------------------------------------------------------------------------------

type RabitState = String
type RabitSymbol = String

type RabitTransiton = ((RabitState, RabitSymbol), RabitState)

type RabitBuchiAutomaton = BuchiAutomaton RabitState RabitSymbol


--------------------------------------------------------------------------------------------------------------
-- Part with the definitions of lexemes
--------------------------------------------------------------------------------------------------------------

lexer = makeTokenParser emptyDef
m_comma = lexeme lexer (char ',')
m_lparen = lexeme lexer (char '(')
m_rparen = lexeme lexer (char ')')
m_ddot = lexeme lexer (char ':')
m_to = lexeme lexer (string "->")
m_quot = lexeme lexer (char '\'')
m_white = whiteSpace lexer
m_state = lexeme lexer (many1 (noneOf " \r\n,->"))
m_label = lexeme lexer (many1 (noneOf " \r\n,->"))

parseFile :: FilePath -> IO RabitBuchiAutomaton
parseFile filename = do
  str <- readFile filename
  return $ parseString str


parseString :: String -> RabitBuchiAutomaton
parseString str = case (parse parseRabitBuchiAutomaton "RabitBuchiAutomataParser" str) of
  Left err   -> error $ show err
  Right res  -> res


--------------------------------------------------------------------------------------------------------------
-- Part with the definitions of parsing rules
--------------------------------------------------------------------------------------------------------------


parseState :: Parser RabitState
parseState = do
  state <- m_state
  return state


parseTransition :: Parser RabitTransiton
parseTransition = do
  symbol <- m_label
  m_comma
  from <- m_state
  m_to
  to <- m_state
  return ((from, symbol), to)


parseRabitBuchiAutomaton :: Parser RabitBuchiAutomaton
parseRabitBuchiAutomaton = do
  ini <- sepEndBy (parseState) newline
  trans <- many (try parseTransition)
  fin <- many (try parseState)
  let iniSet = Set.fromList ini
      finSet = Set.fromList fin
      stSet = Set.union (getAllStates trans) (Set.union iniSet finSet)
      transMap = Aux.mapSetFromList trans
  return $ BuchiAutomaton stSet iniSet finSet transMap


getAllStates :: [RabitTransiton] -> Set.Set RabitState
getAllStates = Set.fromList . concat . map (\((a,_),b) -> [a,b])


--------------------------------------------------------------------------------------------------------------
-- Part with the automata renaming, i.e., [x] -> x
--------------------------------------------------------------------------------------------------------------

rabitStateToInt :: RabitState -> Int
rabitStateToInt st = (read $ init $ tail st) :: Int


rabitBAtoIntBA :: RabitBuchiAutomaton -> BuchiAutomaton Int String
rabitBAtoIntBA (BuchiAutomaton st ini fin tr) = BuchiAutomaton st' ini' fin' tr' where
  st' = Set.map (rabitStateToInt) st
  ini' = Set.map (rabitStateToInt) ini
  fin' = Set.map (rabitStateToInt) fin
  tr' = Map.fromList $ map (\((f,s),t) -> ((rabitStateToInt f, s), Set.map (rabitStateToInt) t)) $ Map.toList tr
