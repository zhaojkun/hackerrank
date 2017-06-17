import Control.Monad
data Tree= Emtpy | Tree Int [Tree] deriving (Show)
data Crumb= Crumb Int [Tree] [Tree] deriving (Show)
type Zipper= (Tree ,[Crumb])
printOut :: Zipper -> IO()
printOut (Tree val _,bs) =
  putStrLn $ show val
change :: Int -> Zipper -> Zipper
change newVal (Tree _ children,bs) =
  (Tree newVal children,bs)
visitLeft :: Zipper -> Zipper
visitLeft (tn,(Crumb val ls rs):bs) =
  let
    ll = init ls
    vv = last ls
  in (vv,Crumb val ll (tn:rs) : bs)
visitRight :: Zipper -> Zipper
visitRight (tn,(Crumb val ls rs):bs) =
  let
    ll = ls ++ [tn]
    rr = tail rs
    t = head rs
  in (t,Crumb val ll rr :bs)
visitChild :: Int -> Zipper -> Zipper
visitChild n (Tree val children,bs) =
  let
    (ls,rsv) = splitAt (n-1) children
    node = head rsv
    rs = tail rsv
  in (node,Crumb val ls rs:bs)
visitParent :: Zipper -> Zipper
visitParent (item,(Crumb val ls rs):bs) = (Tree val (ls ++ [item] ++ rs),bs)
insertLeft :: Int -> Zipper -> Zipper
insertLeft n (t,Crumb val ls rs :bs) =
  let node = Tree n []
  in (t,(Crumb val (ls++[node]) rs):bs)
  
insertRight n (t,Crumb val ls rs :bs) =
  let node = Tree n []
  in (t ,Crumb val ls (node:rs):bs)

insertChild :: Int -> Zipper -> Zipper
insertChild n (Tree val children,bs) =
  (Tree val ((Tree n []):children),bs)
  
delete :: Zipper -> Zipper
delete (_,Crumb val ls rs:bs) =
  (Tree val (ls++rs),bs)

command :: Zipper -> IO Zipper
command z = do
  line <- getLine
  case words line of
    ["change",x] -> return $ change (read x) z
    ["print"] -> do
      printOut z
      return z
    ["visit","left"]    -> return $ visitLeft z
    ["visit","right"]   -> return $ visitRight z
    ["visit","parent"]  -> return $ visitParent z
    ["visit","child",n] -> return $ visitChild (read n) z
    ["insert","left",x] -> return $ insertLeft (read x)  z
    ["insert","right",x]-> return $ insertRight (read x) z
    ["insert","child",x] -> return $ insertChild (read x) z
    ["delete"] -> return $ delete z

initZipper:: Zipper
initZipper =  (Tree 0 [],[])

main :: IO()
main = do
  n <- readLn
  foldl (>>=) (return initZipper) $ replicate n command
  return()

