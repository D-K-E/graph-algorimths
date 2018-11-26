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
  ,VDataL(..)
  ,VerticeID(..)
  ,VerticeIDs(..)
  ,Vertices(..)
  ) where


-- Vertex

data Vertice = Vertice {
  verticeID :: VerticeID
  , verticeData :: VDataL
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

data VerticeIDs = VerticeIDs{
  verticeIDs :: [VerticeID]
  }   deriving(Show, Eq)

data VDataL = VDataL{
  vDataL :: [VData]
  } deriving(Show, Eq)

data Vertices = Vertices {
                         verticesData :: [Vertice]
                         }
              deriving(Show, Eq)


-- Function Definitions Related to Vertices
--
checkVertexById :: Vertice.Vertice -> Vertice.Vertice -> Bool
checkVertexId :: Vertice.Vertice -> VerticeID -> Bool
filterVerticesById :: VerticeID -> Vertices -> Vertice



-- Function Implementations Related to Vertices
--

checkVertexId vertice vId = (verticDonceID vertice) == vId
checkVertexById vertice1 vertice2 = (verticeID vertice1) == (verticeID vertice2)
filterVerticesById vId vertices = let verticeList = verticesData vertices
                                       in filterVerticeListById verticeList vId
                                      where filterVerticeListById (v:vs) vId = if checkVertexId v vId
                                                                               then v
                                                                               else filterVerticeListById vs vId
