main :: IO()
main = do
     x <- getLine
     y <- getLine
     let n = commonPrefix x y
     putStrLn $ (show n) ++ " " ++ (take n x)
     putStrLn $ (show $ length x - n) ++ " " ++ (drop n x)
     putStrLn $ (show $ length y - n) ++ " " ++ (drop n y)

commonPrefix :: String -> String -> Int

commonPrefix [] _ = 0
commonPrefix _ [] = 0
commonPrefix (x:xs) (y:ys) = if x/=y then 0
	     	    	     else 1 + (commonPrefix xs ys)
	     
     