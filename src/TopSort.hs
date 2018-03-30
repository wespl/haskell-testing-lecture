{-# LANGUAGE DeriveFunctor #-}
-- | Adapted from
-- | http://5outh.blogspot.com/2012/12/graphs-and-topological-sorting-in.html
module TopSort where

import Data.List (nub)

data Graph a = Graph {
    vertices :: [a]
  , edges :: [(a, a)]
  } deriving (Show, Functor)

removeEdge :: Eq a => (a, a) -> Graph a -> Graph a
removeEdge x (Graph v e) = Graph v (filter (/= x) e)

connections :: Eq a => ((a, a) -> a) -> a -> Graph a -> [(a, a)]
connections f x (Graph _ e) = filter ((== x) . f) e

-- | Outbound connections
outbound :: Eq b => b -> Graph b -> [(b, b)]
outbound a = connections fst a

-- | Inbound connections
inbound :: Eq b => b -> Graph b -> [(b, b)]
inbound a = connections snd a

graphFromFile :: String -> IO (Graph String)
graphFromFile f = do
  contents <- readFile f
  let info   = map words $ lines contents
      verts = nub . concat $ info
      conns  = map (\[a, b] -> (a, b)) info
      graph  = Graph verts conns
  return graph

tsort :: (Monad m, Eq a) => Graph a -> m [a]
tsort graph  = tsort' [] (noInbound graph) graph
  where noInbound (Graph v e) = filter (flip notElem $ map snd e) v
        tsort' l []    (Graph _ []) = return $ reverse l
        tsort' l []    _            = fail "Cyclic graph"
        tsort' l (n:s) g            = tsort' (n:l) s' g'
          where outEdges = outbound n g
                outNodes = map snd outEdges
                g'       = foldr removeEdge g outEdges
                s' = s ++ filter (null . flip inbound g') outNodes
