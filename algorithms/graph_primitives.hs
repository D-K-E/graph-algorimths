{-
Module      :  Graph Primitive Types
Description :  Trying to define graph primitives in haskell to implement algorithms
Copyright   :  Kaan Eraslan
License     :  Mit

Maintainer  :  Kaan
Stability   :  unstable
Portability :  don't know


-}


-- Module for graph primitives


data Vertex = Vertex {
  inIds :: VertexIDs
  , vertexId :: VertexID
  , vertexData :: VDataL
  , outIds :: VertexIDs
  } deriving (Show)

data VData = String
           | Integer
           | Int
           | Char
           | Bool
           | Double
           deriving(Show)

data VertexID = VertexID Integer
              deriving(Show, Eq)

data VertexIDs = VertexIDs [VertexID]
               deriving(Show, Ord, Eq)

data VDataL = VDataL [VData]
            deriving(Show, Ord, Eq)

data Edge = Edge (Vertex, Vertex)

data Graph = Graph ([Vertex], [Edge])

-- Edge related functions

getFirstV :: Edge -> Vertex

getFirstV anEdge = fst anEdge

getSecondV :: Edge -> Vertex
getSecondV anEdge = snd anEdge

getV_e :: Edge -> Int -> Vertex

getV_e anEdge 1 = getFirstV anEdge
getV_e anEdge 2 = getSecondV anEdge

getVs_e :: Edge -> [Vertex]

getVs_e anEdge = (getV_e anEdge 1):(getV_e anEdge 2):[]

getVs_es :: [Edge] -> [[Vertex]]

getVs_es edgeList = if null edgeList
                          then []
                          else map getVs_e edgeList

getVsFromEdges [] = []

getVList_es :: [Edge] -> [Vertex]

getVList_es edgeList = concat (getVs_es edgeList)

makeGraphFromEdges :: [Edge] -> Graph

-- General Graph functions --

isTrivial :: Graph -> Bool

isTrivial aGraph = if null (fst aGraph)
                      then True
                      else False

getOrder :: Graph -> Integer

getOrder aGraph = if isEmpthGraph aGraph
                  then 0
                  else length (fst aGraph)
