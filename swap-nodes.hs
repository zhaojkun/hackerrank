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
  let t2 = treeSwap k t
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
buildTree lines = build  1
  where
    build :: Int -> Tree
    build (-1) = Empty
    build n =
      let
        (leftVal,rightVal) = lines !! (n-1)
        l = build  leftVal
        r = build rightVal
      in Tree n l r
      
treeSwap :: Int -> Tree -> Tree
treeSwap n t = swap n t
  where
    swap :: Int  -> Tree -> Tree
    swap _  Empty = Empty
    swap 1 (Tree val l r) =
      let
        leftTree = swap  n r
        rightTree = swap n  l
      in Tree val leftTree rightTree
    swap m (Tree val l r) =
      let
        leftTree = swap (m-1) l
        rightTree = swap (m-1) r
      in Tree val leftTree rightTree
