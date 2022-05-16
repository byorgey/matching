-- https://forum.beeminder.com/t/matching-game-perfect-play/10346

module Matching where

import Data.List
import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

-- Compute the number of turns needed to get all the matches given a
-- particular permutation of cards.  Precondition: the list of
-- characters must have even length, with exactly two occurrences of
-- each character.
turns :: [Char] -> Int
turns = go S.empty
  where
    go _ []  = 0
    go _ [_] = 1
    go seen (c1:c2:cs)
      | c1 == c2         = 1 + go seen cs
      | S.member c1 seen = 1 + go seen (c2:cs)
      | S.member c2 seen = 2 + go (S.insert c1 seen) cs
      | otherwise        = 1 + go (S.insert c1 (S.insert c2 seen)) cs

-- Normalize a map of frequencies to give a map of probabilities.
normalize :: Ord a => Map a Integer -> Map a Rational
normalize m = M.map (% t) m
  where
    t = sum m

distributionNaive :: Int -> Map Int Rational
distributionNaive p
  = normalize . M.fromListWith (+) . map ((,1) . turns)
  . permutations
  . concatMap (replicate 2) . take p $ ['A'..]

-- n=2: 1 / 1

-- n=4: 1, 2 / 3

-- n=6: 1, 8, 6 / 3*5

-- n=8: 1, 20, 70, 14   / 3*5*7

-- n=10: 1, 40, 370, 504, 30 / 3*5*7*9


-- 2, 6, 14, 30:  each one is 2^p - 2
-- Lots of central binomial coefficients showing up among the others
