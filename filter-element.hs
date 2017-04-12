import Data.List (nub)
import Data.Map ((!))
import qualified Data.Map as Map

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
        let counts = cntSeq a
        let seq = nub $ filter (\x -> if (Map.findWithDefault 0 x counts) >=k then True else False) $ a
	if null seq then putStrLn "-1" else putStrLn $ unwords $ map show seq
	process (n-1)

cntSeq :: [Int] -> Map.Map Int Int
cntSeq [] = Map.empty
cntSeq (x:xs) = let rs = cntSeq xs
                in  Map.alter ff x rs
  
ff :: Maybe Int -> Maybe Int
ff Nothing = Just 1
ff (Just n) = Just (n+1)

getInts :: String -> [Int]
getInts s = map read $ words s
