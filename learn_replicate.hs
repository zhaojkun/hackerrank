import Control.Monad
main :: IO()
main = do
  let t = 10
  replicateM_ t $ do
    putStrLn "Hello,world"
  
