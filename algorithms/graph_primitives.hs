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

-- Vertex

data Vertex = Vertex {
  getInIds :: VertexIDs
  , getVertexId :: VertexID
  , getVertexData :: VDataL
  , getOutIds :: VertexIDs
  } deriving (Show, Eq)

data VData = String
           | Integer
           | Int
           | Char
           | Bool
           | Double
           deriving(Show, Eq)

data VertexID = VertexID Integer
              deriving(Show, Eq, Ord)

data VertexIDs = VertexIDs{
  getVertexIds :: [VertexID]
  }   deriving(Show, Eq, Ord)

data VDataL = VDataL{
  getVertexDataL :: [VData]
  } deriving(Show, Eq)

data Vertices = Vertices [Vertex]
              deriving(Show, Eq)

-- Edge

data Edge = Edge {
  getEdgeData :: (Vertex, Vertex)
  , getEdgeId :: EdgeID
  , getEdgeFuncs :: FuncIDs
  } deriving(Show, Eq)

data EdgeID = EdgeID Integer
            deriving(Show, Eq)

data EdgeIDs = EdgeIDs [EdgeID]
             deriving(Show, Eq)

-- Functions for mapping functions to edges

data FuncID = FuncID Integer
            deriving(Show, Eq, Ord)

data FuncIDs = FuncIDs{
  getFuncIds :: [FuncID]
  } deriving(Show, Eq, Ord)

data Func = Func{
  getFuncId :: FuncID
  , getFuncParams :: VDataL
  }

-- data FuncData = FuncData FuncId Func

-- Graph

data Graph = Graph (VertexIDs, EdgeIDs)

-- Function declarations

-- Function Types

-- Edge related functions

getFirstV :: Edge -> Vertex
getSecondV :: Edge -> Vertex
getV_e :: Edge -> Int -> Vertex
getVs_e :: Edge -> [Vertex]
getVs_es :: [Edge] -> [[Vertex]]
getVList_es :: [Edge] -> [Vertex]


-- Vertex Related Functions

getInEdges :: Vertex -> [Edge]


-- Graph Related Functions

getOrder :: Graph -> Integer
isTrivial :: Graph -> Bool

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

isTrivial aGraph = if null (fst aGraph)
                      then True
                      else False


getOrder aGraph = if isTrivial aGraph
                  then 0
                  else length (fst aGraph)
