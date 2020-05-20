{-|
  Module      : Simulation.hs
  Description : Functions for working with simulations
  Author      : Vojtech Havlena, April 2019
  License     : GPL-3
-}

module Simulation (
  Simulation(..)
  , repeatUChange
  , simClosure
  , evenCeil
  , evenCeilFin
  , evenFloorFin
  , symmetricSimClosure
  , symClosure
  , rabitSimToInt
) where


import qualified Data.Set.Monad as Set
import qualified Data.Map as Map
import qualified Data.Tuple as Tp
import qualified RabitAutomataParser as RP


data Simulation a =
  Direct (Set.Set (a, a))
  | Delayed (Set.Set (a, a))
  deriving (Show)

repeatUChange f v
  | (f v) == v = v
  | otherwise = repeatUChange f (f v)


simClosure :: (Ord a) => Simulation a -> Set.Set a -> Set.Set a
simClosure (Delayed sim) sset = Set.fromList $ lsim >>= \(x,y) -> if y `elem` sset then return x else [] where
  lsim = Set.toList sim
simClosure (Direct sim) sset = Set.fromList $ lsim >>= \(x,y) -> if y `elem` sset then return x else [] where
  lsim = Set.toList sim


evenCeil :: Int -> Int
evenCeil n = if odd n then n+1 else n


evenCeilFin :: (Ord a) => a -> Set.Set a -> Int -> Int
evenCeilFin s fin n
  | Set.member s fin = if odd n then n+1 else n
  | otherwise = n


evenFloorFin :: (Ord a) => a -> Set.Set a -> Int -> Int
evenFloorFin s fin n
  | Set.member s fin = if odd n then n-1 else n
  | otherwise = n


symmetricSimClosure :: (Ord a) => Simulation a -> Simulation a
symmetricSimClosure (Direct set) = Direct $ symClosure set
symmetricSimClosure (Delayed set) = Delayed $ symClosure set


symClosure :: (Ord a) => Set.Set (a,a) -> Set.Set (a,a)
symClosure set = Set.intersection set inv where
  inv = Set.map (Tp.swap) set


rabitSimToInt :: Simulation String -> Simulation Int
rabitSimToInt (Direct sim) = Direct $ Set.map (\(u,v) -> (RP.rabitStateToInt u, RP.rabitStateToInt v)) sim
rabitSimToInt (Delayed sim) = Delayed $ Set.map (\(u,v) -> (RP.rabitStateToInt u, RP.rabitStateToInt v)) sim
