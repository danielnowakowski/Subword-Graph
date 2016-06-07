-----------------------------------------------------------------------------
--
-- Module      :  Data.SubwordGraph
-- Copyright   :  (c) Adam Bąk, Daniel Nowakowski 2016
-- License     :  BSD3
--
-- Maintainer  :  adambak1992@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- | An implementation of a classic Subword Graph (also known as Directed Acyclic Word Graph).
--   A data structure for solving string related problems on a single word. The implementation is based
--   on a lecture in Polish (with pseudocode): http://smurf.mimuw.edu.pl/node/581
--
-----------------------------------------------------------------------------

module Data.SubwordGraph (
    -- * Types
    Vertex
    , Edge
    , LabeledEdge
    , RootedEdge
    , EdgeType (Solid, Soft)
    , Node
    , SGraph

    -- * Construction
    , construct
    , constructReversed

    -- * Querying
    , elem
    , subwords
    , subwordsNum
    , findNode
    , toWord

    -- * Traversal
    , foldl
    , foldlToNode
    , foldr
    , foldrFromNode
    , topsort

    -- * Modification
    , insert

    -- * Others
    , rootId
    , sinkId
    , nodeId
    , sufId
    , edges
    , nodesNum
    , edgesNum
    , lookupNode
    , findEdge
    , getRootNode
    , getSufNode
    , getSinkNode
) where

import Prelude hiding (elem, fold, foldl, foldr, reverse)
import qualified Prelude as P
import Data.Maybe (isJust, fromMaybe)
import qualified Data.List as DList
import qualified Data.Map as DMap
import qualified Data.IntMap as DIMap
import Control.Monad.State

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | Indicates unique id of a node.
type Vertex = Int

-- | Destination node, edge type.
type Edge = (Vertex, EdgeType)

-- | Edge with its label.
type LabeledEdge a = (a, Edge)

-- | Edge with its startpoint.
type RootedEdge a = (Vertex, a, Edge)

data EdgeType = Solid | Soft deriving (Eq, Show)

-- | Graph structure contains ids of its root and sink as well as the map of all nodes.
data SGraph a = SGraph {
    -- | Returns id of the root.
    rootId :: Vertex,
    -- | Returns id of the sink.
    sinkId :: Vertex,
    nodes  :: DIMap.IntMap (Node a)
} deriving (Eq, Show)

-- | Node structure contains the node's id, suf binding (Nothing iff root) and the map of all outgoing edges.
data Node a = Node {
    -- | Returns id of the node.
    nodeId :: Vertex,
    -- | Returns node's suf binding.
    sufId  :: Maybe Vertex,
    -- | Returns node's outgoing edges.
    edges  :: DMap.Map a Edge
} deriving (Eq, Show)


-----------------------------------------------------------------------------
-- Construction
-----------------------------------------------------------------------------

-- | Index of a root in every graph.
rootIx :: Int
rootIx = 0

-- | Helper for creating nodes.
defaultNode :: Node a
defaultNode = Node {
    nodeId = rootIx,
    sufId  = Nothing,
    edges  = DMap.empty
}

-- | Subword graph for an empty word.
emptyGraph :: SGraph a
emptyGraph = SGraph {
    rootId = rootIx,
    sinkId = rootIx,
    nodes  = DIMap.fromList [ (rootIx, defaultNode :: Node a) ]
}

-- | Constructs a subword graph for a given word. The performance of this function is linear
--  in the length of a word.
construct :: Ord a => [a] -> SGraph a
construct = P.foldl (flip insert) emptyGraph

-- | Constructs a subword graph for a reversed word. The performance of this function is linear
--  in the length of a word.
constructReversed :: Ord a => [a] -> SGraph a
constructReversed = construct . DList.reverse


-----------------------------------------------------------------------------
-- Querying
-----------------------------------------------------------------------------

-- | Indicates whether the subword graph contains the given word. Performance is linear in the length of the word.
elem :: Ord a => [a] -> SGraph a -> Bool
elem word graph = isJust $ findNode word graph

-- | Returns the list of all subwords present in a given subword graph.
subwords :: SGraph a -> [[a]]
subwords = foldr (\(c, _) s1 s2 -> s1 ++ [[c]] ++ map (c:) s2) []

-- | Returns the number of all subwords present in the given subword graph. Performance is linear in the size of the graph.
subwordsNum :: SGraph a -> Int
subwordsNum = foldr (\_ s1 s2 -> 1 + s1 + s2) 0

-- | Finds the given word in the subword graph starting at the given node. Helper function for findNode.
findNodeInternal :: Ord a => [a] -> SGraph a -> Node a -> Maybe (Node a)
findNodeInternal [] _ node = Just node
findNodeInternal (a:ws) graph node
    | not (isEdge node a) = Nothing
    | otherwise = findNodeInternal ws graph node' where
        (vertex, _) = getEdge node a
        node' = getNode vertex graph

-- | Finds the given word in the subword graph. On failure, returns Nothing. On success, returns the
--  node in the subword graph at which the word ends. Performance is linear in the length of the word.
findNode :: Ord a => [a] -> SGraph a -> Maybe (Node a)
findNode word graph = findNodeInternal word graph (getRootNode graph)

-- | Returns a word corresponding the given subword graph. Performance is linear in the length of the word.
toWord :: Ord a => SGraph a -> [a]
toWord g = P.reverse $ head $ toWordWithStack g (rootId g) []

-- | Helper function for toWord.
toWordWithStack :: Ord a => SGraph a -> Vertex -> [a] -> [[a]]
toWordWithStack g nId visited
    | nId == sinkId g = [visited]
    | otherwise = P.foldl f [] (getNodeSolidEdges (getNode nId g))
    where
        f acc (letter, (vertex, _)) = acc ++ toWordWithStack g vertex (letter:visited)


-----------------------------------------------------------------------------
-- Traversal
-----------------------------------------------------------------------------

-- | Folds the edges in a graph, using post-order traversal. Transformer function takes an edge,
--  current node's state and state along the edge. Init state at node is equal to the accumulator.
foldr :: (LabeledEdge a -> b -> b -> b) -> b -> SGraph a -> b
foldr f acc g = fst $ foldrFromNode f acc g (getRootNode g)

-- | Folds the edges in a graph starting at a given node. Returns computed value, as well as a mapping:
--  vertex -> computed value.
foldrFromNode :: (LabeledEdge a -> b -> b -> b) -> b -> SGraph a -> Node a -> (b, DIMap.IntMap b)
foldrFromNode f acc g n = runState (postorderNode f acc g n) DIMap.empty

-- | Process the node in a postorder fashion. Helper function for foldrFromNode.
postorderNode :: (LabeledEdge a -> b -> b -> b) -> b -> SGraph a -> Node a -> State (DIMap.IntMap b) b
postorderNode f acc g n = foldM (postorderGo f g acc) acc $ DMap.toList (edges n)

-- | Helper function for postorderNode.
postorderGo :: (LabeledEdge a -> b -> b -> b) -> SGraph a -> b -> b -> LabeledEdge a -> State (DIMap.IntMap b) b
postorderGo f g initacc acc e = do
    let (_, (dst, ety)) = e
    st <- get
    case DIMap.lookup dst st of
        Nothing -> do
            val <- postorderNode f initacc g (getNode dst g)
            st <- get
            put $ DIMap.insert dst val st
            return (f e acc val)
        Just val -> return (f e acc val)

-- | Folds the edges in a graph, using topological order traversal. Transformer function takes current
--  node's state, current state along the edge, an edge and it produces a new state along the edge.
--  Init state at node is equal to the accumulator.
foldl :: (b -> b -> RootedEdge a -> b) -> b -> SGraph a -> b
foldl f acc g = fst $ foldlToNode f acc g (getSinkNode g)

-- | Folds the edges in a graph up to a given node. Returns computed value, as well as a mapping:
--  vertex -> computed value.
foldlToNode :: (b -> b -> RootedEdge a -> b) -> b -> SGraph a -> Node a -> (b, DIMap.IntMap b)
foldlToNode f acc g n = runState (toporderNode f acc g n (topsort g)) DIMap.empty

-- | Helper function for foldlToNode. It takes a transformer function, accumulator, a subword graph
--  a node up to which we want to continue folding and a list of unprocessed nodes in a topological order.
toporderNode :: (b -> b -> RootedEdge a -> b) -> b -> SGraph a -> Node a -> [Node a] -> State (DIMap.IntMap b) b
toporderNode _ _ _ _ [] = error "toporderNode: invalid end node"
toporderNode f acc g nto (h:t) =
    if nodeId h == nodeId nto
        then getNodeState (nodeId nto) acc
        else do
            let currId = nodeId h
            forM_ (DMap.toList (edges h)) $ \e -> do
                let (c, (dst, ety)) = e
                st <- get
                v1 <- getNodeState currId acc
                v2 <- getNodeState dst acc
                put $ DIMap.insert dst (f v1 v2 (currId, c, (dst, ety))) st
            toporderNode f acc g nto t

-- | Helper function for extracting node's state. It takes a default value which is returned in case of
--  given vertex absence in the state.
getNodeState :: Vertex -> b -> State (DIMap.IntMap b) b
getNodeState idx def = do
    st <- get
    case DIMap.lookup idx st of
        Nothing -> return def
        Just val -> return val

-- | For a given graph returns the list of nodes in a topological order. Performance is linear in the size of the graph.
topsort :: SGraph a -> [Node a]
topsort g = DList.reverse $ toporder g [getRootNode g] [] (countInDegrees g)

-- | Helper function for topsort. It takes a graph, list of unprocessed nodes with indegree equal to 0,
--  current result and a mapping: vertex -> current indegree.
toporder :: SGraph a -> [Node a] -> [Node a] -> DIMap.IntMap Int -> [Node a]
toporder _ [] acc _ = acc
toporder g (h:t) acc m = toporder g t' (h:acc) m' where
    (m', t') = DMap.foldl f (m, t) (edges h)
    f (m1, t1) (dst, _) = (m2, t2) where
        dg = fromMaybe 0 (DIMap.lookup dst m1)
        t2 = if dg == 1 then getNode dst g : t1 else t1
        m2 = DIMap.insert dst (dg - 1) m1

-- | For a given graph returns a mapping: vertex -> indegree. Performance is linear in the size of the graph.
countInDegrees :: SGraph a -> DIMap.IntMap Int
countInDegrees g = P.foldl f initmap nds where
    nds = map snd $ DIMap.toList (nodes g)
    initmap = DIMap.fromList [ (nodeId a, 0) | a <- nds]
    f acc nd = DMap.foldl (\a (dst, _) -> DIMap.adjust (1+) dst a) acc (edges nd)


-----------------------------------------------------------------------------
-- Modification
-----------------------------------------------------------------------------

-- | Adds an element to a given graph creating a new graph for a word with this element appended.
insert :: Ord a => a -> SGraph a -> SGraph a
insert c g = splitByNode fixedG c fixedW where
    (newSinkG, sinkNum) = addNewNode g
    edgeToSink = (sinkNum, Solid)
    oldSinkNum = sinkId g
    newSinkEdgeG = (setEdge oldSinkNum c edgeToSink newSinkG) { sinkId = sinkNum }
    w = getSufNode (getNode oldSinkNum newSinkEdgeG) newSinkEdgeG
    (fixedG, fixedW) = fixSufBindings isEdge sinkNum Soft w c newSinkEdgeG

-- | Performs fixing nodes in the suf bindings chain after inserting a new node. Function takes a predicate which
--  indicates the end of fixing, node id where we need to redirect edges, edge type to redirect,
--  starting node, edge label and a current graph.
fixSufBindings
    :: Ord a => (Node a -> a -> Bool)
    -> Vertex
    -> EdgeType
    -> Maybe (Node a)
    -> a
    -> SGraph a
    -> (SGraph a, Maybe (Node a))
fixSufBindings _ _ _ Nothing _ g = (g, Nothing)
fixSufBindings edgePred redirectTo edgeType (Just w) c g
    | edgePred w c = (g, Just w)
    | otherwise = fixSufBindings edgePred redirectTo edgeType w' c g' where
        g' = setEdge (nodeId w) c (redirectTo, edgeType) g
        w' = getSufNode w g' -- from a new graph get vertex with (sufId w)

-- | Another necessary update of a graph after inserting a new node.
--  Performs splitting given node in two after fixSufBindings to preserve correctness.
--  For further details please visit: http://smurf.mimuw.edu.pl/node/581 (lecture in Polish, contains pseudocode).
splitByNode :: Ord a => SGraph a -> a -> Maybe (Node a) -> SGraph a
splitByNode g _ Nothing = changeSinkSuf g (rootId g)
splitByNode g c (Just w) =
    case getEdge w c of
        (v, Solid) ->  changeSinkSuf g v
        (v, Soft) -> g7 where
            (g1, v') = addNewNode g
            g2 = P.foldl redirectEdge g1 $ DMap.toList (edges (getNode v g1))

            redirectEdge :: Ord a => SGraph a -> (a, Edge) -> SGraph a
            redirectEdge g (c, (u, _)) = setEdge v' c (u, Soft) g

            g3 = setEdge (nodeId w) c (v', Solid) g2
            g4 = changeSinkSuf g3 v'
            g5 = changeSufNode g4 v' (getSufNode (getNode v g4) g4)
            g6 = changeSuf g5 v (Just v')
            sufW = getSufNode w g6 -- from a new graph get vertex with (sufId w)
            (g7, _) = fixSufBindings isNodeEdgeSolid v' Soft sufW c g6


-- | Updates the suf binding of a graph's sink node with a given id.
changeSinkSuf :: SGraph a -> Vertex -> SGraph a
changeSinkSuf g suf = changeSuf g (sinkId g) (Just suf)

-- | Updates the suf binding of a node with a given id of 'v'.
changeSuf :: SGraph a -> Vertex -> Maybe Vertex -> SGraph a
changeSuf g v Nothing = changeSufNode g v Nothing
changeSuf g v (Just suf) = changeSufNode g v $ Just (getNode suf g)

-- | Updates the suf binding of a node with a given id with a node represented by 'sufNode' value.
changeSufNode :: SGraph a -> Vertex -> Maybe (Node a) -> SGraph a
changeSufNode g v sufNode = updateNode v vNodeNewSuf g where
    vnode = getNode v g
    vNodeNewSuf =
        vnode { sufId = sufNode >>= return . nodeId }

-- | Adds node to a given graph.
addNode :: Node a -> SGraph a -> SGraph a
addNode n g = g { nodes = DIMap.insert k n (nodes g) } where
    k = nodeId n

-- | Adds new node to a given graph. Returns new graph and new node's id.
addNewNode :: SGraph a -> (SGraph a, Int)
addNewNode g = (addNode n g, k) where
    n = defaultNode { nodeId = k }
    k = nodesNum g

-- | Adds or updates an edge from a node with a given index in a given graph.
setEdge :: Ord a => Int -> a -> Edge -> SGraph a -> SGraph a
setEdge ix c e g = g { nodes = DIMap.insert ix n' (nodes g) } where
    n = getNode ix g
    n' = n { edges = DMap.insert c e (edges n) }

-- | Updates node with a given index in a graph with a new value.
updateNode :: Int -> Node a -> SGraph a -> SGraph a
updateNode ix n g = g { nodes = DIMap.insert ix n (nodes g) }


-----------------------------------------------------------------------------
-- Others
-----------------------------------------------------------------------------

-- | Returns number of nodes for a given graph.
nodesNum :: SGraph a -> Int
nodesNum = DIMap.size . nodes

-- | Returns number of edges for a given graph.
edgesNum :: SGraph a -> Int
edgesNum = DIMap.foldr ((+) . DMap.size . edges) 0 . nodes

-- | Returns node with a given index. Error when such doesn't exist.
getNode :: Int -> SGraph a -> Node a
getNode ix g = fromMaybe (error $ "there is no node with id: " ++ show ix) (lookupNode ix g)

-- | Returns node with a given index. Nothing iff such does not exist.
lookupNode :: Int -> SGraph a -> Maybe (Node a)
lookupNode ix g = DIMap.lookup ix (nodes g)

-- | For a given graph returns its root node.
getRootNode :: SGraph a -> Node a
getRootNode g = getNode (rootId g) g

-- | For a given node in a given graph returns its suf link node.
getSufNode :: Node a -> SGraph a -> Maybe (Node a)
getSufNode n g = do
    sid <- sufId n
    return $ getNode sid g

-- | For a given graph returns its sink node.
getSinkNode :: SGraph a -> Node a
getSinkNode g = getNode (sinkId g) g

-- | Looks up an edge from a given node with a given label.
findEdge :: Ord a => Node a -> a -> Maybe Edge
findEdge n c = DMap.lookup c (edges n)

-- | Returns an edge from a given node with a given label. Error when the specific edge does not exist.
getEdge :: Ord a => Node a -> a -> Edge
getEdge n c = fromMaybe (error $ "there is no given egde from node with id: " ++ show (nodeId n) ) (findEdge n c)

getNodeSolidEdges :: Ord a => Node a -> [(a, Edge)]
getNodeSolidEdges n = P.filter (\(_, edge) -> isEdgeSolid edge) (DMap.toList (edges n))

-- | Checks whether there is an edge from a given node with a given label.
isEdge :: Ord a => Node a -> a -> Bool
isEdge n c = isJust $ findEdge n c

isEdgeSolid :: Edge -> Bool
isEdgeSolid (_, et) = et == Solid

-- | Checks whether an edge from a given node with a given label is solid. Error when such does not exist.
isNodeEdgeSolid :: Ord a => Node a -> a -> Bool
isNodeEdgeSolid n c = isEdgeSolid $ getEdge n c

