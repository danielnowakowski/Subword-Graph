{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Adam Bąk, Daniel Nowakowski 2016
-- License     :  BSD3
--
-- Maintainer  :  adambak1992@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- | Testing basic properties of Data.SubwordGraph via QuikCheck.
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import System.Exit
import Control.Monad (unless)
import Test.QuickCheck

import Data.List

import qualified Data.SubwordGraph as DSGraph


-- | List of all nonempty sublists of a given list.
sublists :: Eq a => [a] -> [[a]]
sublists = nub . f where
    f = concatMap (tail . inits) . tails

-- | Test whether an empty string is a substring of a given string.
prop_ElemEmpty1 :: String -> Bool
prop_ElemEmpty1 w = DSGraph.elem "" (DSGraph.construct w)

-- | Test whether an empty word is a subword of a given word over integers.
prop_ElemEmpty2 :: [Int] -> Bool
prop_ElemEmpty2 w = DSGraph.elem [] (DSGraph.construct w)

-- | Test whether a string is its own substring.
prop_ElemWord1 :: String -> Bool
prop_ElemWord1 w = DSGraph.elem w (DSGraph.construct w)

-- | Test whether a word over integers is its own subword.
prop_ElemWord2 :: [Int] -> Bool
prop_ElemWord2 w = DSGraph.elem w (DSGraph.construct w)

-- | Test elem function.
prop_Elem :: String -> String -> Bool
prop_Elem a b = DSGraph.elem a (DSGraph.construct b) == isInfixOf a b

-- | Test subwords function.
prop_Subwords :: String -> Bool
prop_Subwords w = sort l1 == sort l2 where
    l1 = DSGraph.subwords (DSGraph.construct w)
    l2 = sublists w

-- | Test subwordsNum function.
prop_SubwordsNum :: String -> Bool
prop_SubwordsNum w = DSGraph.subwordsNum (DSGraph.construct w) == length (sublists w)

-- | Test insert function.
prop_Insert :: [Int] -> Int -> Bool
prop_Insert w a = DSGraph.construct (w ++ [a]) == DSGraph.insert a (DSGraph.construct w)

-- | Test word extraction from a graph.
prop_ConstrToWord :: String -> Bool
prop_ConstrToWord w = DSGraph.toWord (DSGraph.construct w) == w

-- | Test linear number of nodes. Implication below is a simple theorem.
prop_NodesNum :: String -> Property
prop_NodesNum w = (n >= 3) ==> (v <= 2 * n - 1) where
    g = DSGraph.construct w
    n = length w
    v = DSGraph.nodesNum g

-- | Test linear number of edges. Implication below is a simple theorem.
prop_EdgesNum :: String -> Property
prop_EdgesNum w = (n >= 3) ==> (e <= 3 * n - 4) where
    g = DSGraph.construct w
    n = length w
    e = DSGraph.edgesNum g


return []
main = do
    allPass <- $quickCheckAll
    unless allPass exitFailure


