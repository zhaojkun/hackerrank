import qualified Data.Map as Map
main = do
  let m0 = Map.empty
  let m1 = Map.insert "k1" 7 m0
  let m2 = Map.alter ff "k2" m1
  let m3 = Map.alter ff "k1" m2
  putStrLn $ show $ m3


ff :: Maybe Int -> Maybe Int
ff Nothing = Just 1
ff (Just n)= Just (n+1)
