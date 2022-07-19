import Text.CSV

-- Parses a CSV file
--main :: IO ()
main = do
  let fileName = "input.csv"
  input <- readFile fileName
  let csv = parseCSV fileName input
  either handleError doWork csv
  let r = head csv
  print ("jkljlk")
  
handleError csv = putStrLn "not a CSV"
doWork items = filter (\x -> length x == 2) items
	--res = doWork csv
--print (head res)

--doWork :: [Record] -> [Record]
--doWork csv = tail $ filter (\x -> length x == 2) csv

-- Finds oldest person.
findOldest :: [Record] -> Record
findOldest [] = []
findOldest items = foldl1 (\a x -> if age x > age a then x else a) items

age [a,b] = toInt b

toInt :: String -> Int                               
toInt = read
