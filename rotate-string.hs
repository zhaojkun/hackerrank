import Data.List

main :: IO()

main = do
     x <- getLine
     processCases $ read x


processCases :: Int -> IO()
processCases 0 = return ()
processCases n = do
	     x <- getLine
	     putStrLn $ intercalate " " $ rotate x
	     processCases (n-1)


rotate :: String -> [String]
rotate [] = []
rotate s = rotateInner (length s) s


rotateInner :: Int -> String -> [String]
rotateInner 0 _ = []
rotateInner n (x:xs) = (xs ++ [x]) : (rotateInner (n-1) (xs ++ [x]))