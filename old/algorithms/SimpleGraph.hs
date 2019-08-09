{-
Module      :  Graph Primitive Types
Description :  Trying to define graph primitives in haskell to implement
               algorithms
Copyright   :  Kaan Eraslan
License     :  Mit

Maintainer  :  Kaan
Stability   :  unstable
Portability :  don't know
-}
module Graph(
  Graph(..)
  ) where

import qualified Vertice
import qualified Edge

data Graph = Graph {vertices :: [Vertice.Vertice]
                    ,edges :: [Edge.Edge]
                    } deriving(Show, Eq, Ord)

-- Graph Related Functions to implement

isComplete :: Graph -> Bool -- vertices are pairwise adjacent
isRegular :: Graph -> Bool
isDisjoint :: Graph -> Graph -> Bool
isFHomomorphic :: Graph -> Graph -> Bool
isIsomorphic :: Graph -> Graph -> Bool

-- Implemented Functions
getOrder :: Graph -> Int
getOrder graph = length (vertices graph)

isTrivial :: Graph -> Bool
isTrivial g = getOrder g == 0 || getOrder g == 1


getEdgeNumber :: Graph -> Int
getEdgeNumber graph = length (edges graph)



edgeVertexRatio :: Graph -> Float  -- get edge vertex ratio of graph
edgeVertexRatio graph
  | null vertices graph = 0
  | otherwise = edges graph / vertices graph

-- Add graph to one another
merge2Graphs :: Graph -> Graph -> Graph

merge2Graphs g1 g2 = Graph {
                         vertices = vertices g1 ++ vertices g2,
                         edges = edges g1 ++ edges g2
                        }

mergeGraphs :: [Graph] -> Graph

mergeGraphs (g:gs) = foldl merge2Graphs g gs

getCommonSubgraph :: Graph -> Graph -> Graph
getCommonVertices :: Graph -> Graph -> [Vertice.Vertice]
getCommonVertices g1 g2 = Vertice.filterVerticesByVertices (vertices g1) (vertices g2)

getCommonEdges :: Graph -> Graph -> [Edge.Edge]
getCommonEdges g1 g2 = Edge.filterEdgesByEdges (edges g1) (edges g2)

getCommonSubgraph g1 g2 = Graph {
  vertices = getCommonVertices g1 g2,
  edges = getCommonEdges g1 g2
                                }
-- outputs the vertice with degree with the specified

isVerticeListIndependent :: Graph -> [Vertice.Vertice] -> Bool

isVerticeListIndependent g vs = let es = edges g
                                in if length Edge.filterAdjacentVertices vs es > 0
                                   then True
                                   else False
                                -- Graph Related Function Implementations
isVerticeIndependent :: Graph -> Vertice.Vertice -> Bool

isVerticeIndependent g v = let vs = [v]
                           in isVerticeListIndependent g vs

isEdgeListIndependent :: Graph -> [Edge.Edge] -> Bool

isEdgeListIndependent g es = if length Edge.filterAdjacentEdges es > 0
                             then True
                             else False

isEdgeIndependent :: Graph -> Edge.Edge -> Bool

isEdgeIndependent g e = let es = [e]
                        in isEdgeListIndependent g es

getVertexDegree :: Vertice.Vertice -> Graph -> Integer
getVertexDegree vertex graph
  | null (edges graph) = 0
  | otherwise = length (filterEdgesWithVertexById vertex (edges graph))

compareVerticeDegree2Degree :: (a -> Bool) -> Vertice.Vertice -> Graph -> Int -> Bool
compareVerticeDegree2Degree op v1 graph degree
  | op == (<) = getVertexDegree v1 graph < degree
  | op == (>) = getVertexDegree v1 graph > degree
  | op == (<=) = getVertexDegree v1 graph <= degree
  | op == (>=) = getVertexDegree v1 graph >= degree
  | op == (==) = getVertexDegree v1 graph == degree

compare2VerticesByDegree :: (a -> Bool) -> Vertice.Vertice -> Vertice.Vertice -> Graph -> Bool
compare2VerticesByDegree op v1 v2 graph
  | op == (<) = getVertexDegree v1 graph < getVertexDegree v2 graph
  | op == (>) = getVertexDegree v1 graph > getVertexDegree v2 graph
  | op == (==) = getVertexDegree v1 graph == getVertexDegree v2 graph
  | op == (<=) = getVertexDegree v1 graph <= getVertexDegree v2 graph
  | op == (>=) = getVertexDegree v1 graph >= getVertexDegree v2 graph

filterVerticeListByDegree :: (a -> Bool) -> [Vertice.Vertice] -> Int -> Graph -> [Vertice.Vertice]
filterVerticeListByDegree op (v:vs) degree graph = if compareVerticeDegree2Degree op v graph degree
                                                   then v : filterVerticeListByDegree op vs graph degree
                                                   else filterVerticeListByDegree op vs degree graph

filterGraphByVerticeDegree :: (a -> Bool) -> Graph -> Int -> Graph
filterGraphByVerticeDegree op g1 degree

-- quick sort implementation for sorting verticelist

sortVerticeListByDegree :: [Vertice.Vertice] -> Graph -> [Vertice.Vertice]
sortVerticeListByDegree (v:vs) graph
  | null (v:vs) = []
  | otherwise = let left = filterVerticeListByDegree (<=) vs (getVertexDegree v graph) graph
                    equal = filterVerticeListByDegree (==) vs (getVertexDegree v graph) graph
                    right = filterVerticeListByDegree (>) vs (getVertexDegree v graph) graph
                in sortVerticeListByDegree left graph  ++ equal ++ sortVerticeListByDegree right graph

sortGraphByDegree :: Graph -> Graph  -- order graph vertices by their degree
sortGraphByDegree graph = let vs = vertices graph
                              es = edges graph
                          in Graph { vertices = sortVerticeListByDegree vs graph,
                                     edges = es
                                   }

averageDegree :: Graph -> Integer  -- get average degree of graph
averageDegree graph = let verticeList = Vertice.Vertices.verticesData (vertices graph)
                          edgeList = Edge.Edges.edgesData(edges graph)
                          order = getOrder graph
                      in getDegreeSum 0 verticeList edgeList / order
                         where
                           getDegreeSum accu (v:verticeList)
                             | null (v:verticeList) = accu
                             | otherwise = getDegreeSum (accu + getVertexDegree v edgeList) verticeList

maxDegree :: Graph -> Integer -- get maximum degree of graph
maxDegree graph = getVertexDegree (last (sortVerticeListByDegree (vertices graph) graph)) graph

minDegree :: Graph -> Integer -- get minimum degree of graph
minDegree graph = getVertexDegree (head (sortVerticeListByDegree (vertices graph) graph)) graph

