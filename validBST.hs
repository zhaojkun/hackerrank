import Control.Monad
main :: IO ()
main = do
  n <- read <$> getLine
  replicateM_ n $ do
    getLine
    line <- getLine
    let x = map read $ words line
    putStrLn  $ (\x -> if x then "YES" else "NO") $ validBST x 
  
validBST :: [Int] -> Bool
validBST [] = True
validBST (x:xs) =
  let
    (x1,x2) = break (>x) xs
    b1 = all (>x) x2
  in b1 && (validBST x1) && (validBST x2)

