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
  ,EdgeID(..)
  ) where

import qualified Vertice

data Edge = Edge {
  edgeData :: (Vertice.VerticeID, Vertice.VerticeID)
  , edgeID :: EdgeID
  } deriving(Show, Eq)

data EdgeID = EdgeID Integer
            deriving(Show, Eq)

-- Edge Related Function Declarations

checkVertexIdInEdge :: Vertice.Vertice -> Edge -> Bool
getVertexIdFromEdgeFst :: Edge -> Vertice.VerticeID
getVertexIdFromEdgeScnd :: Edge -> Vertice.VerticeID
filterEdgesWithVertexById :: Vertice.Vertice -> [Edge] -> [Edge]

-- Edge Related Function implementations

getVertexIdFromEdgeFst anEdge = fst (edgeData anEdge)
getVertexIdFromEdgeScnd anEdge = snd (edgeData anEdge)

checkVertexIdInEdge vertex anEdge = let fstCondition = getVertexIdFromEdgeFst anEdge == Vertice.verticeID vertex
                                        scndCondition = getVertexIdFromEdgeScnd anEdge == Vertice.verticeID vertex
                                    in (fstCondition || scndCondition)

filterEdgesWithVertexById vertex [] = []
filterEdgesWithVertexById vertex (edge:edges) = filter (checkVertexIdInEdge vertex) edges
