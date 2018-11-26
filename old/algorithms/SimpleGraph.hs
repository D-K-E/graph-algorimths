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

-- Graph Related Functions

getOrder :: Graph -> Int
getEdgeNumber :: Graph -> Int
isTrivial :: Graph -> Bool
minDegree :: Graph -> Integer -- get minimum degree of graph
maxDegree :: Graph -> Integer -- get maximum degree of graph
averageDegree :: Graph -> Integer  -- get average degree of graph
edgeVertexRatio :: Graph -> Integer  -- get edge vertex ratio of graph
sortGraphByDegree :: Graph -> Graph  -- order graph vertices by their degree
getVertexDegree :: Vertice.Vertice -> Graph -> Integer

sortVerticeListByDegree :: [Vertice.Vertice] -> Graph -> [Vertice.Vertice]
compare2VerticesByDegree :: (a -> Bool) -> Vertice.Vertice -> Vertice.Vertice -> Graph -> Bool

filterVerticeListByDegree :: (a -> Bool) -> [Vertice.Vertice] -> Int -> Graph -> [Vertice.Vertice]
compareVerticeDegree2Degree :: (a -> Bool) -> Vertice.Vertice -> Graph -> Int -> Bool
-- outputs the vertice with degree with the specified

-- Graph Related Function Implementations

getVertexDegree vertex graph
  | null (edges graph) = 0
  | otherwise = length (filterEdgesWithVertexById vertex (edges graph))

compareVerticeDegree2Degree op v1 graph degree
  | op == (<) = (getVertexDegree v1 graph) < degree
  | op == (>) = (getVertexDegree v1 graph) > degree
  | op == (<=) = (getVertexDegree v1 graph) <= degree
  | op == (>=) = (getVertexDegree v1 graph) >= degree
  | op == (==) = (getVertexDegree v1 graph) == degree

compare2VerticesByDegree op v1 v2 graph
  | op == (<) = (getVertexDegree v1 graph) < (getVertexDegree v2 graph)
  | op == (>) = (getVertexDegree v1 graph) > (getVertexDegree v2 graph)
  | op == (==) = (getVertexDegree v1 graph) == (getVertexDegree v2 graph)
  | op == (<=) = (getVertexDegree v1 graph) <= (getVertexDegree v2 graph)
  | op == (>=) = (getVertexDegree v1 graph) >= (getVertexDegree v2 graph)

filterVerticeListByDegree op v:vs degree graph = if compareVerticeDegree2Degree op v graph degree:
                                                 then v : filterVerticeListByDegree op v graph degree
                                                 else filterVerticeListByDegree op vs degree graph

-- quick sort implementation for sorting verticelist

sortVerticeListByDegree (v:vs) graph
  | null (v:vs) = []
  | otherwise = let degree = getVertexDegree v graph
                in divideAndConquer vs degree graph
  where divideAndConquer xs degree graph = let left = filterVerticeListByDegree (<) vs degree graph
                                               equal = filterVerticeListByDegree (==) vs degree graph
                                               right = filterVerticeListByDegree (>) vs degree graph
                                           in left ++ [v] ++ equal ++ right

getOrder graph = length (vertices graph)
getEdgeNumber graph = length (edges graph)

averageDegree graph = let verticeList = Vertice.Vertices.verticesData (vertices graph)
                          edgeList = Edge.Edges.edgesData(edges graph)
                          order = getOrder graph
                      in (getDegreeSum 0 verticeList edgeList) / order
                         where
                           getDegreeSum accu v:verticeList
                             | null v:verticeList = accu
                             | otherwise = getDegreeSum (accu + getVertexDegree v edgeList) verticeList

maxDegree graph = getVertexDegree (tail (sortVerticeListByDegree (vertices graph) graph)) graph
minDegree graph = getVertexDegree (head (sortVerticeListByDegree (vertices graph) graph)) graph
