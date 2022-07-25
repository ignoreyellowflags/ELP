import Data.Graph
import qualified Data.Set as Set
import Data.Array
import Data.List (sort) 
import Control.Applicative


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
  
  let neighborVertex = map (\idx -> (nodeFromVertex idx, getNgh $ nodeFromVertex idx) ) $ indices myGraph
  
  let neighborSet    = map (\elem -> (fst elem, fmap nodeFromVertex $ snd elem) ) neighborVertex :: [(LNode [Char],[LNode [Char] ])]
  
  let influenceSet   = map (\elem -> ( getNid $ fst elem , (influence 100) <$> [fst elem] <*> snd elem) )  neighborSet
 
  putStrLn $ "kl " ++ (show influenceSet) 

mytest a b = res + 10
  where
    amod = a + 1
    bmod = b + 3
    res  = a * b

ldense nodenum dgr = dgr / (nodenum - 1)

getLbl (a,_,_) = a

getNid (_,a,_) = a

getNgh (_,_,a) = a 

data WNode = WNode {
                   lbl :: [Char],
                   nid :: Integer,
                   ngh :: [Integer]
                   } deriving Show 




--neighbors'  :: (Vertex -> LNode [Char]) -> Vertex -> [LNode [Char] ]
--neighbors' f nid = fmap f neighborIDs
--  where 
--    neighborIDs = getthd $ nodeFromVertex nid


--neighborSearch graph idx = Prelude.map (\i -> (i, (!) graph i) ) idx


type LNode a = (a, Vertex, [Vertex]) 

sim u v = setInter / setUnion
  where
    uset = Set.fromList $ getNgh u
    vset = Set.fromList $ getNgh v
    setInter = fromIntegral . Set.size $ uset `Set.intersection` vset
    setUnion = (+1) . fromIntegral . Set.size $ uset `Set.union` vset

influence nodenum  u v = sim u v * (/) vDense uDense
  where
    vDense = ldense nodenum (fromIntegral $ length (getNgh v))
    uDense = ldense nodenum (fromIntegral $ length (getNgh u))


--sim' :: (Fractional b, Ord a) => [a] -> [a] -> b
sim' u v = setInter / setUnion
  where 
    uset     = Set.fromList $ u
    vset     = Set.fromList $ v
    setInter = fromIntegral . Set.size $ uset `Set.intersection` vset
    setUnion = (+1) . fromIntegral . Set.size $ uset `Set.union` vset
    
influence' nodenum u v = sim' u v * (/) vDense uDense
  where
    vDense = ldense nodenum (fromIntegral $ length v ) 
    uDense = ldense nodenum (fromIntegral $ length u )



{-sim u v = nominator / denominator
  where
    uNgh = Set.fromList $ snd u
    vNgh = Set.fromList $ snd v 
    nominator = fromIntegral . Set.size $ uNgh `Set.intersection` vNgh
    denominator = (+1) . fromIntegral . Set.size $ uNgh `Set.union` vNgh


influence nodenum  u v = sim u v * (/) vDense uDense
  where
    uDense = ldense nodenum (fromIntegral ( length $ snd u) )
    vDense = ldense nodenum (fromIntegral ( length $ snd v) )
-}
{-influence' nodenum  u v = (fst u, WNode (fst u) res )
  where
    uDense = ldense nodenum (fromIntegral ( length $ snd u) )
    vDense = ldense nodenum (fromIntegral ( length $ snd v) )
    res    = sim u v * (/) vDense uDense
-}

--phi gamma infvalue = alpha * exp (-gamma * (1 - infvalue) / infvalue ) 

updateOrder xs = map abs $ getZipList $ (-) <$> ZipList normalized_influence <*> ZipList delta_it
  where
    normalized_influence = map (/acc_influence) xs
    acc_influence        = sum xs
    n          = fromIntegral (length xs)
    delta_it   = map (/n) xs        
       


-- | Compute the median of a list
median :: (Ord a, Fractional a) => [a] -> a
median x =
   if odd n
     then sort x !! (n `div` 2)
     else ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
    where n = length x


{-data Focal a b c =  Focal {
                          nid    :: a, -- node ID
                          label  :: b, -- node label
                          weight :: c  -- node weight
                          } deriving Show
-}

--modWeight f a = WNode (nid a) (f $ weight a)

--infdet graph = Prelude.map (\elem -> (influence' 100) <$>  [elem] <*> neighborSearch graph (snd elem) ) $ assocs graph


{-gamma f xs = 1 / (median eta)
  where
    eta = Prelude.map (\elem -> f . weight $ snd elem ) xs
 -}
   
{-massassign xs = Prelude.map (\x -> modWeight (phi gammaV) x) xs
  where
   gammaV = gamma (\y -> (1-y) / y) xs
-}
