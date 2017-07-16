import Control.Monad
calcps = 0:1: zipWith (\x y -> 2*y - x +3) calcps (tail calcps)
ps n = calcps !! n

ps' :: Int -> Int
ps' n  = div (n * (3* n- 1)) 2

main :: IO()
main = do
  n <- readLn :: IO Int
  replicateM_ n $ do
    m <- readLn :: IO Int
    putStrLn $ show $ ps' m
