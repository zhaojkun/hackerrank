import Control.Monad
main :: IO()
main = do
  n <- readLn
  vals <- readLines n
  let t = buildTree vals
  m <- readLn
  foldl (>>=) (return t) $ replicate m process
  return ()
  
process :: Tree -> IO Tree
process t = do
  k <- readLn
  let t2 = treeSwap t k
  inorderP t2
  putStr "\n"
  return t2

      
inorderP :: Tree -> IO()
inorderP Empty = return ()
inorderP (Tree val l r) = do
  inorderP l
  putStr $ (show  val) ++ " "
  inorderP r
  

readLines :: Int -> IO [(Int,Int)]
readLines 0 = return []
readLines n = do
  line <- getLine
  rs <-  readLines (n-1)
  let [a,b] = words line
  return ((read a,read b):rs)


data Tree = Empty | Tree {value::Int ,left::Tree,right::Tree} deriving (Show)

buildTree :: [(Int,Int)]  -> Tree
buildTree lines = buildTreeHelper lines 1

buildTreeHelper:: [(Int,Int)] -> Int -> Tree
buildTreeHelper _ (-1) = Empty
buildTreeHelper lines n =
  let
    (leftVal,rightVal) = lines !! (n-1)
    l = buildTreeHelper lines leftVal
    r = buildTreeHelper  lines rightVal
  in Tree n l r

treeSwap :: Tree -> Int -> Tree
treeSwap t n = treeSwapHelper t n n

treeSwapHelper :: Tree -> Int -> Int -> Tree
treeSwapHelper Empty _ _ = Empty
treeSwapHelper (Tree val l r) n 1 =
  let
    leftTree = treeSwapHelper r n n
    rightTree = treeSwapHelper l n n
  in
    Tree val leftTree rightTree
treeSwapHelper (Tree val l r ) n m =
  let
    leftTree = treeSwapHelper l n (m-1)
    rightTree = treeSwapHelper r n (m-1)
  in
    Tree val leftTree rightTree
    

