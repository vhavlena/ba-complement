{-|
Module      : Simulation.hs
Description : Functions for working with simulations
Author      : Vojtech Havlena, April 2019
License     : GPL-3
-}

module Simulation (
  repeatUChange
  , simClosure
  , evenCeil
  , evenCeilFin
  , evenFloorFin
  , DelaySim
) where


import qualified Data.Set as Set
import qualified Data.Map as Map


type DelaySim a = Set.Set (a, a)

repeatUChange f v
  | (f v) == v = v
  | otherwise = repeatUChange f (f v)


simClosure :: (Ord a) => DelaySim a -> Set.Set a -> Set.Set a
simClosure sim sset = Set.fromList $ lsim >>= \(x,y) -> if y `elem` sset then return x else [] where
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
