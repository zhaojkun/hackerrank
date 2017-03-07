main =  do
     line <- getLine
     let n =  read line :: Int
     readCases n

isFunction :: [[Int]] -> Bool
isFunction []  =  True
isFunction (x:xs) = (all (isFnTest x) xs) && (isFunction xs)

isFnTest :: [Int] -> [Int] -> Bool
isFnTest x y  = ((x !! 0) /=  (y !! 0)) || ((x !! 1)==(y !! 1))

showRes :: Bool -> String
showRes True = "YES"
showRes False = "NO"

readCases :: Int -> IO()
readCases 0 = return ()
readCases n = do
	  line <- readInts
	  c <- readLines (head line)
	  let t = isFunction c
	  putStrLn $ showRes t
	  readCases (n-1)
	  
	  
readLines :: Int -> IO [[Int]]
readLines 0 = return ([])
readLines n = do
	  x <- readInts
	  xs <- readLines (n-1)
	  return (x:xs)


readInts :: IO [Int]
readInts = fmap (map read.words) getLine