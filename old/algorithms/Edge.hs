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

module Edge(
  Edge(..)
  ,Edges(..)
  ,EdgeID(..)
  ,EdgeIDs(..)
  ,getVertexDegree
  ,filterEdgesByEdge
  ,filterEdgesByEdges
  ,filterEdgesByVertices
  ,filterEdgesWithVertexById
  , isIncident
  , isAdjacentEdge
  , isAdjacentVertice
  , filterAdjacentEdges
  , filterAdjacentVertices
  ) where

import qualified Vertice

data Edge = Edge {
  edgeData :: (Vertice.VerticeID, Vertice.VerticeID)
  , edgeID :: EdgeID
  } deriving(Show, Eq)

data Edges = Edges {
  edgesData :: [Edge]
  } deriving(Show, Eq, Ord)

data EdgeID = EdgeID Integer
            deriving(Show, Eq)

data EdgeIDs = EdgeIDs [EdgeID]
             deriving(Show, Eq)

-- Edge Related Function Declarations
hasAdjacentVertex :: Vertice.Vertice -> [Vertice.Vertice] -> [Edge] -> Bool
getAdjacentVertices :: Vertice.Vertice -> [Vertice.Vertice] -> [Edge] -> [Vertice.Vertice]

hasAdjacentEdge :: Edge -> [Edge] -> Bool
getAdjacentEdges :: Edge -> [Edge] -> [Edge]
getVertexDegree :: Vertice.Vertice -> Edges -> Int

-- Edge Related Function implementations
getVertexIdFromEdgeFst :: Edge -> Vertice.VerticeID
getVertexIdFromEdgeScnd :: Edge -> Vertice.VerticeID
getVertexIdFromEdgeFst anEdge = fst (edgeData anEdge)
getVertexIdFromEdgeScnd anEdge = snd (edgeData anEdge)

getVertexIdFromEdge :: Int -> Edge -> Vertice.VerticeID

getVertexIdFromEdge pos e
  | pos == 0 = getVertexIdFromEdgeFst e
  | pos == 1 = getVertexIdFromEdgeScnd e
  | pos > 1 = error "position arg should be either 0 or 1"



isIncident :: Vertice.Vertice -> Edge -> Bool
isIncident v e = Vertice.verticeID v == getVertexIdFromEdgeFst e || Vertice.verticeID v == getVertexIdFromEdgeScnd e

isAdjacentEdge :: Edge -> Edge -> Bool
isAdjacentEdge e1 e2
  | getVertexIdFromEdge 0 e1 == getVertexIdFromEdge 0 e2 = True
  | getVertexIdFromEdge 1 e1 == getVertexIdFromEdge 0 e2 = True
  | getVertexIdFromEdge 0 e1 == getVertexIdFromEdge 1 e2 = True
  | getVertexIdFromEdge 1 e1 == getVertexIdFromEdge 1 e2 = True
  | otherwise = False

filterAdjacentEdges :: [Edge] -> [Edge]

filterAdjacentEdges (e1:e2:es)
  | null es = []
  | null (e1:e2) = []
  
getVertexDegree vertex edges
  | null edges = 0
  | otherwise = length (filterEdgesWithVertexById vertex edges)

checkEdgeIdInEdge :: EdgeID -> Edge
checkEdgeIdInEdge eid edge = eid == edgeID edge

filterEdgesByEdgeId :: EdgeID -> [Edge] -> [Edge]
filterEdgesByEdgeId eid (e:es)
  | null (e:es) = []
  | otherwise = if checkEdgeIdInEdge eid e
                then e : filterEdgesByEdgeId eid es
                else filterEdgesByEdgeId es

filterEdgesByEdge :: Edge -> [Edge] -> [Edge]
filterEdgesByEdge e es
  | null es = []
  | otherwise = filterEdgesByEdgeId (edgeID e) es

filterEdgesByEdges :: [Edge] -> [Edge] -> [Edge]
filterEdgesByEdges (e1:es1) es2
  | null (e1:es1) = es2
  | null es2 = []
  | otherwise = filterEdgesByEdge e1 es2 ++ filterEdgesByEdges es1 es2

-- Filter edges with vertex


checkVertexIdInEdge :: Vertice.Vertice -> Edge -> Bool
checkVertexIdInEdge vertex anEdge = let fstCondition = getVertexIdFromEdgeFst anEdge == Vertice.verticeID vertex
                                        scndCondition = getVertexIdFromEdgeScnd anEdge == Vertice.verticeID vertex
                                    in (fstCondition || scndCondition)

filterEdgesWithVertexById :: Vertice.Vertice -> Edges -> Edges
filterEdgesWithVertexById vertex [] = []
filterEdgesWithVertexById vertex (anEdge:edgeList) = if checkVertexIdInEdge vertex anEdge
                                                     then anEdge : filterEdgesWithVertexById vertex edgeList
	                        					     else filterEdgesWithVertexById vertex edgeList

filterEdgesByVertices :: [Vertice.Vertice] -> [Edge] -> [Edge]
filterEdgesByVertices (v:vs) es
  | null (v:vs) = es
  | null es = []
  | otherwise = filterEdgesWithVertexById v es ++ filterEdgesByVertices vs es

isAdjacentVertice :: Vertice.Vertice -> Vertice.Vertice -> [Edge] -> Bool
isAdjacentVertice v1 v2 es = let v1edges = filterEdgesWithVertexById v1 es
                                 v2edges = filterEdgesWithVertexById v2 es
                             in length filterEdgesByEdges v1edges v2edges > 0

filterAdjacentVertices :: [Vertice.Vertice] -> [Edge] -> [Vertice.Vertice]

filterAdjacentVertices (v1:v2:vs) es 
  | null (v1:v2) = []
  | null vs = []
  | null (v2:vs) = []
 
