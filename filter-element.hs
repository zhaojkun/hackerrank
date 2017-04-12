import Data.List (nub)
import Data.Map ((!))
import qualified Data.Map as Map
import Control.Monad
main :: IO()
main = do
  n <- read <$> getLine
  replicateM_ n $ do
    	[_,k] <- getInts <$> getLine
	a <- getInts <$> getLine
        let counts = cntSeq a
        let seq = nub $ filter (\x -> if (Map.findWithDefault 0 x counts) >=k then True else False) $ a
	if null seq then putStrLn "-1" else putStrLn $ unwords $ map show seq

cntSeq :: [Int] -> Map.Map Int Int
cntSeq [] = Map.empty
cntSeq (x:xs) = let rs = cntSeq xs
                in  Map.alter ff x rs
  
ff :: Maybe Int -> Maybe Int
ff Nothing = Just 1
ff (Just n) = Just (n+1)

getInts :: String -> [Int]
getInts s = map read $ words s
