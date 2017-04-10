main :: IO()
main = do
     line <- getLine
     let n = read line :: Int
     process n
    


process :: Int -> IO()
process 0 = return ()
process n = do
	l1 <- getLine
	l2 <- getLine
	let kk = getInts l1
	let a = getInts l2
	let k = kk !! 1
	let seq = filter (\(x,n)-> if n>=k then True else False) $ countSeq a
	if null seq then putStrLn "-1" else putStrLn $ unwords $ map (show . fst) seq
	process (n-1)

countSeq :: [Int] -> [(Int,Int)]
countSeq [] = []
countSeq all@(x:_) =
	 let
		(n,l) = filterElem x all
	in (x,n) : countSeq l

filterElem :: Int -> [Int] -> (Int,[Int])
filterElem n [] = (0,[])
filterElem n (x:xs) =
	   let
		(m,ys) = filterElem n xs
	   in if n==x then (m+1,ys) else (m,x:ys)


getInts :: String -> [Int]
getInts s = map read $ words s