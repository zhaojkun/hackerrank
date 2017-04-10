import Data.List

main :: IO()

main = do
     x <- getLine
     processCases $ read x


processCases :: Int -> IO()
processCases 0 = return ()
processCases n = do
	     x <- getLine
	     putStrLn $ intercalate " " $ take (length x) $ rotate x
	     processCases (n-1)

rotate :: String -> [String]
rotate (x:xs) = let cur = xs ++ [x] in cur : rotate cur

