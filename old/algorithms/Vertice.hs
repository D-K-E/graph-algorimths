{-
Module      :  Graph Primitive Types
Description :  Trying to define graph primitives in haskell to implement algorithms
Copyright   :  Kaan Eraslan
License     :  Mit

Maintainer  :  Kaan
Stability   :  unstable
Portability :  don't know
-}

module Vertice (
  Vertice (..)
  ,VData (..)
  ,VerticeID(..)
  ,filterVerticesByVertices
  ,filterVerticesByVertice
  ) where


-- Vertex

data Vertice = Vertice {
  verticeID :: VerticeID
  , verticeData :: [VData]
  } deriving (Show, Eq)

data VData = String
           | Integer
           | Int
           | Char
           | Bool
           | Double
           deriving(Show, Eq)

data VerticeID = VerticeID Integer
              deriving(Show, Eq)

-- Function Definitions Related to Vertices
--
checkVertexById :: Vertice.Vertice -> Vertice.Vertice -> Bool
checkVertexId :: Vertice.Vertice -> VerticeID -> Bool
filterVerticesById :: VerticeID -> [Vertice] -> [Vertice]



-- Function Implementations Related to Vertices
--

checkVertexId vertice vId = verticeID vertice == vId
checkVertexById vertice1 vertice2 = verticeID vertice1 == verticeID vertice2
filterVerticesById vId (v:vs)
  | null (v:vs) = []
  | otherwise = if checkVertexId v vId
                then [v]
                else filterVerticesById vId vs

filterVerticesByVertice :: Vertice -> [Vertice] -> [Vertice]
filterVerticesByVertice v1 vs
  | null vs = []
  | otherwise = filterVerticesById (verticeID v1) vs


filterVerticesByVertices :: [Vertice] -> [Vertice] -> [Vertice]
filterVerticesByVertices (v1:vs1) vs2
  | null (v1:vs1) = vs2
  | null vs2 = []
  | otherwise = filterVerticesByVertice v1 vs2 ++ filterVerticesByVertices
  vs1 vs2
