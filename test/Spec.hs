module Main where

import Test.Hspec
import Test.QuickCheck
import Data.List
import TopSort

instance (Eq a, Arbitrary a) => Arbitrary (Graph a) where
  arbitrary = do
    es <- nub <$> arbitrary
    sinks <- oneof [pure [], arbitrary] -- occasionally add sink nodes
    return $ Graph (nub $ sinks ++ concatMap (\(x, y) -> [x, y]) es) es
  shrink g@(Graph vs es) = xs ++ ys
    where xs = map (`removeEdge` g) es
          ys = map (`removeVertexEdges` g) vs

falseTest :: Graph Int -> Bool
falseTest (Graph vs es) = not (5 `elem` vs)

comesBefore :: Eq a => a -> a -> [a] -> Bool
comesBefore first second xs = case (i, j) of
    (Just i', Just j') -> i < j
    _ -> False
  where i = findIndex (== first) xs
        j = findIndex (== second) xs

respectTopOrder :: Graph Int -> Bool
respectTopOrder g@(Graph vs es) = case topSort g of
  Nothing -> True -- cyclic graph so vacuously true
  Just xs -> all (\(x,y) -> comesBefore x y xs) es

containAll :: Graph Int -> Bool
containAll g@(Graph vs es) = case topSort g of
  Nothing -> True -- cyclic graph so vacuously true
  Just xs -> all (`elem` xs) vs && all (`elem` vs) xs

vertexForEdges :: Graph Int -> Bool
vertexForEdges (Graph vs es) = all (\(x,y) -> x `elem` vs && y `elem` vs) es

uniqueVertices :: Graph Int -> Bool
uniqueVertices (Graph vs _) = length vs == length (nub vs)

uniqueEdges :: Graph Int -> Bool
uniqueEdges (Graph _ es) = length es == length (nub es)

-- | Erdős–Gyárfás conjecture:
-- Every graph with minimum degree 3 contains a simple cycle
-- whose length is a power of two.
-- This test will just check if there is a cycle,
-- because we didn't write a function to get the length of a cycle.
erdosGyarfas :: Graph Int -> Bool
erdosGyarfas g@(Graph vs es) =
    if length vs < 3 || not (all hasThree vs)
    then True -- vacuously true
    else case topSort g of
           Nothing -> True -- cyclic!
           Just _ -> False -- counterexample!
  where
    hasThree v = length (inbound v g ++ outbound v g) >= 3

main :: IO ()
main = hspec $ do
  it "should have all vertices only once in the vertices list" $
    property uniqueVertices
  it "should have all edges only once in the edges list" $
    property uniqueEdges
  it "should have all edge members in the vertices list" $
    property vertexForEdges
  it "should contain all vertices in the result and vice versa" $
    property containAll
  it "should have x come before y for all edges (x, y)" $
    property respectTopOrder
  it "should satisfy the baby Erdős-Gyárfás conjecture" $
    property erdosGyarfas
  it "should not have the vertex 5 (bogus test to see if shrinking works)" $
    property falseTest
