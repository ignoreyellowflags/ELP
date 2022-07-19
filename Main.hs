import Data.Graph
import qualified Data.Set as Set
import Data.Array
import Data.List (sort) 

alpha = 1
--Representing a graph from a list of edges

{-myGraph :: Graph
myGraph = buildG bounds edges
  where
     bounds = (1,4)
     edges  = [ (1,3), (1,4)
              , (2,3), (2,4)
              , (3,4) ]
main = do
  print $ "The edges are " ++ (show.edges) myGraph
  print $ "The vertices are " ++ (show.vertices) myGraph
  -}

   

myGraph :: Graph
(myGraph,nodeFromVertex,vertexFromKey) = graphFromEdges [ ("Moscow", 0, [2,3] )
                           , ("Tver", 1, [2,3] )
                           , ("Spb", 2, [0,1,3]   )
                           , ("Volgograd", 3, [0,1,2] )  ]

main = do 
  putStrLn $ "The edges are " ++ (show.edges) myGraph
  putStrLn $ "The vertices are " ++ (show.vertices) myGraph


mytest a b = res + 10
  where
    amod = a + 1
    bmod = b + 3
    res  = a * b

ldense nodenum dgr = dgr / (nodenum - 1)


neighborSearch graph idx = Prelude.map (\i -> (i, (!) graph i) ) idx

sim u v = nominator / denominator
  where
    uNgh = Set.fromList $ snd u
    vNgh = Set.fromList $ snd v 
    nominator = fromIntegral . Set.size $ uNgh `Set.intersection` vNgh
    denominator = (+1) . fromIntegral . Set.size $ uNgh `Set.union` vNgh


influence nodenum  u v = sim u v * (/) vDense uDense
  where
    uDense = ldense nodenum (fromIntegral ( length $ snd u) )
    vDense = ldense nodenum (fromIntegral ( length $ snd v) )

influence' nodenum  u v = (fst u, WNode (fst u) res )
  where
    uDense = ldense nodenum (fromIntegral ( length $ snd u) )
    vDense = ldense nodenum (fromIntegral ( length $ snd v) )
    res    = sim u v * (/) vDense uDense


phi gamma infvalue = alpha * exp (-gamma * (1 - infvalue) / infvalue ) 

-- | Compute the median of a list
median :: (Ord a, Fractional a) => [a] -> a
median x =
   if odd n
     then sort x !! (n `div` 2)
     else ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
    where n = length x

data WNode a b = WNode {nid :: Vertex, weight :: b} 
  deriving (Eq,Ord,Show)

modWeight f a = WNode (nid a) (f $ weight a) 

--data WEdge a b c  = WEdge {src :: a, dst :: b, weight :: c}
--  deriving (Eq, Ord, Show)  
--individuation xs = 

infdet graph = Prelude.map (\elem -> (influence' 100) <$>  [elem] <*> neighborSearch graph (snd elem) ) $ assocs graph


massassign xs = Prelude.map (\x -> modWeight (phi medianValue) x) xs 
  where
    medianValue = median $ Prelude.map (\x -> weight x) xs

