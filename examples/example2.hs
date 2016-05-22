-- This module solves the following problem (in linear time):
-- For a given pair of strings: A, B and a positive natural number K, calculate the number of
-- subwords of A that occur in B exactly K times.
--
-- input 1:
--  acb
--  abab
--  2
-- output 1:
--  2
--
-- input 2:
--  abc
--  abab
--  2
-- output 2:
--  3
--
-- In a solution we assume that neither '#' nor '$' occur in given strings.
--

import Data.SubwordGraph hiding (foldl, foldr)
import qualified Data.SubwordGraph as DSG
import Data.Maybe (fromMaybe)
import qualified Data.IntMap as DIMap

marker1 = '#'
marker2 = '$'

-- | Linear time solution.
solve :: String -> String -> Int -> Int
solve a b k = foldl f4 0 (DIMap.toList ndsRchblty) where
    g = construct $ a ++ [marker1] ++ b ++ [marker2]
    aend = fromMaybe (error "no way!") $ findNode (a ++ [marker1]) g

    -- for each vertex it's number of associated subwords
    ndsWordsNum = snd $ DSG.foldlToNode f1 0 g (getSinkNode g)

    -- for each vertex it's reachability of '#', and the number of ways to '$' but not through '#'
    ndsRchblty = snd $ DSG.foldrFromNode f2 (False, 0) g (getRootNode g)

    f1 st1 st2 (src, _, _) = if src == rootId g then st2 + 1 else st1 + st2

    f2 (_, (dst, _)) st1 st2 = (fst st1 || fst st2 || dst == nodeId aend, f3 dst (snd st1) (snd st2))

    f3 dst st1 st2
        | dst == sinkId g = 1 + st1
        | dst == nodeId aend = st1
        | otherwise = st1 + st2

    f4 acc (nid, (hreach, dways))
        | hreach && (dways == k) = acc + fromMaybe 0 (DIMap.lookup nid ndsWordsNum)
        | otherwise = acc


-- | Brute force solution.
bruteForce :: String -> String -> Int -> Int
bruteForce a b k = foldl f 0 (subwords ga) where
    ga = construct a
    gb = construct (b ++ [marker1])
    f acc w = if countRepeats w gb == k then acc + 1 else acc

-- | Counts how many times does the given word occur in the given graph.
countRepeats :: String -> SGraph Char -> Int
countRepeats w g =
    case findNode w g' of
        Nothing -> 0
        Just n -> fst $ foldrFromNode f 0 g' n
    where
        g' = DSG.insert marker1 g
        f (_, (dst, _)) st1 st2 =
            if dst == sinkId g'
                then 1 + st1
                else st1 + st2

rint :: String -> Int
rint = read

main :: IO ()
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    let k = rint c
    print $ solve a b k
    return ()

