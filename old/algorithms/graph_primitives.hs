{-
Module      :  Graph Primitive Types
Description :  Trying to define graph primitives in haskell to implement algorithms
Copyright   :  Kaan Eraslan
License     :  Mit

Maintainer  :  Kaan
Stability   :  unstable
Portability :  don't know
-}

-- TODO See functions, we should work with identifiers instead of
-- the data itself, for not consuming a lot of memory

-- Module for graph primitives

-- Data Declarations

-- Edge

-- Continue dividing types into submodules

import qualified Vertex
import qualified Edges

-- Functions for mapping functions to edges

data FuncID = FuncID Integer
            deriving(Show, Eq, Ord)

data FuncIDs = FuncIDs{
  getFuncIds :: [FuncID]
  } deriving(Show, Eq, Ord)

data Func = Func{
  getFuncId :: FuncID
  , getFuncParams :: [Edge]
  }

-- data FuncData = FuncData FuncId Func

-- Graph

data Graph = Graph (VertexIDs, EdgeIDs)

-- Path

data Path = Path (VertexIDs, EdgeIDs)

-- Function declarations

-- Function Types

-- Edge related functions

getFirstV :: Edge -> Vertex  -- Get the first vertice of an edge
getSecondV :: Edge -> Vertex -- Get the second vertice of an edge
getV_e :: Edge -> Int -> Vertex
getVs_e :: Edge -> [Vertex]  -- Get vertices of an edge
getVs_es :: [Edge] -> [[Vertex]]  -- Get vertices of an edge list
getVList_es :: [Edge] -> [Vertex]


-- Vertex Related Functions

getInEdges :: Vertex -> [Edge]


-- Function Implementations

-- Edge related functions

getFirstV anEdge = fst (getEdgeData anEdge)

getSecondV anEdge = snd (getEdgeData anEdge)


getV_e anEdge 1 = getFirstV anEdge
getV_e anEdge 2 = getSecondV anEdge


getVs_e anEdge = (getV_e anEdge 1):(getV_e anEdge 2):[]


getVs_es edgeList = if null edgeList
                          then []
                          else map getVs_e edgeList

getVsFromEdges [] = []

--makeGraphFromEdges :: [Edge] -> Graph

getVList_es edgeList = concat (getVs_es edgeList)

-- Vertex Related Functions

-- Graph Related Functions

-- TODO: Get min degree of graph
-- TODO: Get max degree of graph
-- TODO: Get average degree of graph
-- TODO: Get edge vertex ratio of graph

isTrivial aGraph = if null (fst aGraph)
                      then True
                      else False


getOrder aGraph = if isTrivial aGraph
                  then 0
                  else length (fst aGraph)

--
