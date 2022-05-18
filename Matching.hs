-- https://forum.beeminder.com/t/matching-game-perfect-play/10346

module Matching where

import Control.Arrow ((&&&))
import Data.Array
import Data.Function (fix)
import Control.Monad (msum)
import Data.Bifunctor (second)
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

-- All (2p-1)!! permutations of AABBCC... unique up to relabelling.
pairingOrders :: Int -> [[Char]]
pairingOrders = map interpCode . genCodes
  where

    -- gen p generates all (2p-1)!! lists of length p where the first
    -- element is chosen from 1..(2p-1), the second from 1..(2p-3),
    -- etc.
    genCodes :: Int -> [[Int]]
    genCodes 0 = [[]]
    genCodes p = [ k:ks | k <- [0 .. 2*p-2], ks <- genCodes (p-1) ]

    -- Interpret one of the codes output by genCode as a permutation of
    -- AABBCC... unique up to relabelling.  In particular, the first
    -- element of the output is always labelled A; then the first
    -- number in the list is taken to be the index of the other A in
    -- the remaining elements.  The next unlabelled item is then B,
    -- and the next number in the list is interpreted as the index
    -- (among the yet-unlabelled items) of the matching B; and so on.
    -- For example, [1,2,0] corresponds to ABACCB:
    --
    --   - The first item is A
    --
    --   - The 1 means that the matching A is at index 1 among the
    --     remaining slots: A_A___
    --
    --   - The next unlabelled slot is labelled B:  ABA___
    --
    --   - The matching B is at index 2 in the unlabelled slots: ABA__B
    --
    --   - The C's go in the remaining slots:  ABACCB
    --
    interpCode :: [Int] -> [Char]
    interpCode = map snd . sortOn fst . go 'A' [0..]
      where
        go curLabel emptySlots [] = []
        go curLabel (s1:emptySlots) (i:is) = (s1,curLabel) : (s2,curLabel) : go (succ curLabel) emptySlots' is
          where
            (s2,emptySlots') = removeIx i emptySlots

removeIx :: Int -> [a] -> (a,[a])
removeIx 0 (a:as) = (a,as)
removeIx i (a:as) = second (a:) (removeIx (i-1) as)

distribution :: Int -> Map Int Rational
distribution
  = normalize . M.fromListWith (+) . map ((,1) . turns)
  . pairingOrders

distributionNumerators :: Int -> Maybe [Integer]
distributionNumerators p = sequenceA . map (extractInt . (* (dfac (2*p-1) % 1))) . M.elems . distribution $ p
  where
    extractInt r
      | denominator r == 1 = Just $ numerator r
      | otherwise          = Nothing

dfac :: Int -> Integer
dfac n
  | n <= 0 = 1
  | otherwise = fromIntegral n * dfac (n-2)

expected :: Int -> Rational
expected p = sum $ zipWith (*) (map (% dfac (2*p - 1)) ns) (map fromIntegral [p .. 2*p - 1])
  where
    Just ns = distributionNumerators p

-- Lazy version of multiplication that short-circuits if the first argument is 0;
-- needed to avoid making some recursive calls in cases where the probability is 0
infixl 7 .*
0 .* _ = 0
x .* y = x * y

-- p t m k = probability of taking exactly t additional turns when there are m pairs left and we know k cards
p :: Integer -> Integer -> Integer -> Rational
p 0 0 _ = 1
p t _ _ | t <= 0 = 0
p t 1 _ = if t == 1 then 1 else 0
p t m k
  = k % (2*m - k) .* p (t-1) (m-1) (k-1)
  + (1 - k % (2*m - k)) .*
      ( (k % (2*m - k - 1)) .* p (t-2) (m-1) k
      + (1 % (2*m - k - 1)) .* p (t-1) (m-1) k
      + (1 - (k+1) % (2*m - k - 1)) .* p (t-1) m (k+2)
      )

q :: (Integer,Integer,Integer) -> Rational
q = memoFix ((-1,0,0), (200,100,100)) q'
  where
    q' _ (0,0,_) = 1
    q' _ (t,_,_) | t <= 0 = 0
    q' _ (t,1,_) = if t == 1 then 1 else 0
    q' r (t,m,k)
      = k % (2*m - k) .* r (t-1,m-1,k-1)
      + (1 - k % (2*m - k)) .*
        ( (k % (2*m - k - 1)) .* r (t-2,m-1,k)
          + (1 % (2*m - k - 1)) .* r (t-1,m-1,k)
          + (1 - (k+1) % (2*m - k - 1)) .* r (t-1,m,k+2)
        )


toTable :: Ix i => (i, i) -> (i -> a) -> Array i a
toTable rng f = array rng (map (id &&& f) (range rng))

fromTable :: Ix i => Array i a -> (i -> a)
fromTable = (!)

memo :: Ix i => (i, i) -> (i -> a) -> (i -> a)
memo rng = fromTable . toTable rng

memoFix :: Ix i => (i, i) -> ((i -> a) -> (i -> a)) -> (i -> a)
memoFix rng f = fix (memo rng . f)
