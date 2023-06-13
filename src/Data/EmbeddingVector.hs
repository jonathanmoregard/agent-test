module Data.EmbeddingVector (EmbeddingVector, embeddingVector, cosineSimilarity) where

import Data.Vector (Vector)
import Data.Vector qualified as V

newtype EmbeddingVector
  = EmbeddingVector (Vector Double)

embeddingVector :: Vector Double -> EmbeddingVector
embeddingVector = EmbeddingVector

-- Calculate the cosine similarity between two embedding vectors
cosineSimilarity :: EmbeddingVector -> EmbeddingVector -> Double
cosineSimilarity (EmbeddingVector vec1) (EmbeddingVector vec2) = dotProduct / (magnitude1 * magnitude2)
  where
    dotProduct = V.sum $ V.zipWith (*) vec1 vec2
    magnitude1 = vectorMagnitude vec1
    magnitude2 = vectorMagnitude vec2
    -- Calculate the magnitude of a vector
    vectorMagnitude :: Vector Double -> Double
    vectorMagnitude vec = sqrt $ V.sum $ V.map (\x -> x * x) vec