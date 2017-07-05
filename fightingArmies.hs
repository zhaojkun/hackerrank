import Data.List
import Debug.Trace
import Data.Array
ll :: Array Int Int -> Int
ll a  =
  let
    (m,n)= bounds a
  in n-m
     
  
findStrongest :: Int -> Array Int [Int] -> Int
findStrongest n armies =  armies ! (n-1) !! 0
  
strongestDied :: Int -> Array Int [Int]  -> Array Int [Int]
strongestDied n armies   =
  let
    d = armies ! (n-1)
  in armies // [(n-1,tail d)]
  
recruit :: Int -> Int -> Array Int [Int] ->  Array Int [Int]
recruit i c armies =
  let
    dd = armies ! (i-1)
    ee = insertBy (flip compare ) c dd
  in armies // [(i-1,ee)]
  
merge :: Int -> Int -> Array Int [Int] ->  Array Int [Int]
merge i j armies=
  let
    li = armies ! (i-1)
    lj = armies ! (j-1)
    mm = mergeList li lj
  in armies // [(i-1,mm),(j-1,[])]
  
  
mergeList :: [Int] -> [Int] -> [Int]
mergeList a [] = a
mergeList [] a = a
mergeList (x:xs) (y:ys) =
  if x > y
  then x:y:left
  else y:x:left
  where
    left = mergeList xs ys

process :: Array Int [Int] -> IO (Array Int [Int])
process armies = do
  dd <- map read <$> words <$> getLine
  processline dd armies
  where 
    processline (1:[i])  armies = do
      putStrLn . show $   findStrongest i  armies
      return (armies)
    processline (2:[i]) armies = do
      return (strongestDied i armies)
    processline (3:[i,c]) armies = do
      return (recruit i c armies)
    processline (4:[i,j]) armies = do
      let res = merge i j armies
      return (res)
  
main :: IO()
main = do
  [n,q]  <- map read <$> words <$> getLine
  foldl (>>=) (return $ initData n) $ replicate q process
  return()

initData :: Int -> Array Int [Int]
initData n  = array (0,n-1) [(i,[])| i<-[0..(n-1)]]
