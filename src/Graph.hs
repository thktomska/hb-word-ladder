{-# LANGUAGE ScopedTypeVariables #-}

module Graph
  ( DiGraph
  , addEdge
  , addEdges
  , buildDiGraph
  , children
  , deleteNodes
  , bfsSearch
  ) where

import qualified Data.AssocMap as M
import qualified Data.List as L

type DiGraph a = M.AssocMap a [a]

addEdge :: (Eq a) => (a, a) -> DiGraph a -> DiGraph a
addEdge (node, child) = M.alter insertEdge node
  where
    insertEdge Nothing = Just [child]
    insertEdge (Just nodes) =
      Just (L.nub (child : nodes))

addEdges :: (Eq a) => [(a, a)] -> DiGraph a -> DiGraph a
addEdges edges graph = foldr addEdge graph edges

buildDiGraph :: (Eq a) => [(a, [a])] -> DiGraph a
buildDiGraph nodes = go nodes M.empty
  where
    go [] graph = graph
    go ((key, value) : xs) graph = M.insert key value (go xs graph)

children :: (Eq a) => a -> DiGraph a -> [a]
children = M.findWithDefault []

deleteNodes :: Eq a => [a] -> DiGraph a -> DiGraph a
deleteNodes edges graph = foldr M.delete graph edges

type SearchState a = ([a], DiGraph a, DiGraph a)
data SearchResult a = Unsuccessful | Successful (DiGraph a)

bfsSearch :: forall a. Eq a => DiGraph a -> a -> a -> Maybe [a]
bfsSearch graph start end
  | start == end = Just [start]
  | otherwise =
    case bfsSearch' ([start], graph, M.empty) of
          Successful preds -> Just (findSolution preds)
          Unsuccessful -> Nothing
  where
    bfsSearch' :: SearchState a -> SearchResult a
    bfsSearch' ([], _, _preds) = Unsuccessful
    bfsSearch' (frontier, graph', preds) =
      let
          frontier' = L.concatMap snd children'
          children' =
            L.map
              (\node -> (node, L.filter (`M.member` reducedGraph) (children node graph')))
              frontier
          reducedGraph = deleteNodes frontier graph'
          preds' = addMultiplePredecessors children' preds
       in if end `L.elem` frontier'
            then Successful preds'
            else bfsSearch' (frontier', reducedGraph, preds')

    addMultiplePredecessors :: [(a, [a])] -> DiGraph a -> DiGraph a
    addMultiplePredecessors [] graph' = graph'
    addMultiplePredecessors ((nodeKey, children') : nodes) graph' =
      addMultiplePredecessors nodes (go nodeKey children' graph')
      where
        go _ [] graph'' = graph''
        go nodeKey' (child : children'') graph'' = go nodeKey' children'' (addEdge (child, nodeKey') graph'')

    findSolution :: DiGraph a -> [a]
    findSolution g = L.reverse (go end)
      where
        go key =
          case children key g of
            [] -> [key]
            (ch : _) -> key : go ch
