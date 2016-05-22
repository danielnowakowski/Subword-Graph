import Data.SubwordGraph hiding (foldr)
import qualified Data.SubwordGraph as DSG
import Data.Char

-- | Finds all occurences of the given word in the given graph. Returns the list of all end positions
--  of a word in a text corrsponding given subword graph.
findOccurences :: String -> SGraph Char -> [Int]
findOccurences w g =
    case findNode w g' of
        Nothing -> []
        Just n -> map (m-) $ fst $ foldrFromNode f [] g' n
    where
        g' = DSG.insert '#' g
        t = toWord g'
        m = length t
        f (_, (dst, _)) st1 st2 =
            if dst == sinkId g'
                then [1]
                else st1 ++ map (1+) st2


-- | Counts how many times does the given word occur in the given graph.
countRepeats :: String -> SGraph Char -> Int
countRepeats w g =
    case findNode w g' of
        Nothing -> 0
        Just n -> fst $ foldrFromNode f 0 g' n
    where
        g' = DSG.insert '#' g
        f (_, (dst, _)) st1 st2 =
            if dst == sinkId g'
                then 1 + st1
                else st1 + st2


-- | Returns the list of all suffixes present in a given subword graph. The ordering is as in a suffix array.
suffixes :: SGraph Char -> [String]
suffixes g = init $ map init $ DSG.foldr f [] g' where
    endmarker = chr 256
    g' = DSG.insert endmarker g
    f (c, (dst, _)) s1 s2 =
        if dst == sinkId g'
            then s1 ++ [[c]]
            else s1 ++ map (c:) s2
