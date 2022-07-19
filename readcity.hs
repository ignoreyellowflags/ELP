import Text.CSV
import Data.Graph as GraphLib
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (fromMaybe)


--main :: IO ()
main = do
  let mydata  = [ [0,2], [0,1], [1,0], [1,3], [2,0], [2,3], [3,2], [3,1] ]
  let mapcity = Map.fromList [(0,"Moscow"), (1,"Tver"), (2, "SPb"), (3,"Volgograd")]
  let adjacencyList =  Map.assocs $ Map.fromListWith (++) $ Prelude.map (\r -> (head r, tail r) ) mydata

  let adjacencyList'= Prelude.map (\x -> ( bloodhound mapcity (fst x), fst x, snd x) ) adjacencyList
--  print(hh)
--  let rgraph = fst $ GraphLib.graphFromEdges' adjacencyList'
  let (rGraph,nodeFromVertex,vertexFromKey) = GraphLib.graphFromEdges adjacencyList'

  putStrLn $ "The edges are " ++ (show.edges) rGraph 
  putStrLn $ "The vertives are " ++ (show.vertices) rGraph

bloodhound :: Ord k => Map k [Char] -> k -> [Char]
bloodhound records record = fromMaybe "Unknown" $ Map.lookup record records

--ldensity :: Fractional a => a -> a -> a
ldense nodenum dgr = dgr / (nodenum -1) 

--sim :: (Fractional b, Ord a) => [a] -> [a] -> b
sim u v =  nominator / denominator
  where
    uNgh = Set.fromList $ snd u
    vNgh = Set.fromList $ snd v
    nominator   = fromIntegral . Set.size $ uNgh `Set.intersection` vNgh
    denominator = (+1) . fromIntegral . Set.size $ uNgh `Set.union` vNgh

influence nodenum u v = sim u v * (/) vDense uDense 
  where
    uDense = ldense nodenum (fromIntegral ( length $ snd u) )
    vDense = ldense nodenum (fromIntegral ( length $ snd v) )

--neighborSearch :: Ord k => Map k b -> [k] -> [b]
neighborSearch graph idx = Prelude.map (\i -> (!) graph i) idx
