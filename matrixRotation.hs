import Debug.Trace
import Data.List
main :: IO()
main = do
  [m,n,r]  <- map read <$> words <$> getLine
  matrix <- map (map read ) <$> map words <$> lines <$> getContents
  let mm =  matrixSplit matrix
  let mm2 = rotateMatrix r mm
  let res = matrixJoin m n mm2
  mapM_  (putStrLn . intercalate  " " . map show) $ res
  
rotateMatrix:: Int -> [[Int]] -> [[Int]]
rotateMatrix _ [[]] = [[]]
rotateMatrix n m =
  map (\x -> rotateLine x) m
  where
    rotateLine :: [Int] -> [Int]
    rotateLine  []  = []
    rotateLine  m =
      let
        r =  n `mod` (length m)
        (f,l) = splitAt ((length m ) - r) m
      in l ++ f
  
matrixSplit ::  [[Int]] -> [[Int]]
matrixSplit [] = []
matrixSplit [[]] = [[]]
matrixSplit m =
  let
    firstLine = head m
    leftElems = map head m
    rightElems = map last m
    lastLine = last m
    inner =   map (init .tail) (init $ tail m )
    d  = leftElems ++ (tail lastLine) ++ (tail $ reverse rightElems) ++ (tail $ reverse $ tail firstLine)
  in (d:matrixSplit inner)


matrixWrap :: Int -> Int -> [Int] -> [[Int]] -> [[Int]]
matrixWrap _ _ [] m = m
matrixWrap h w m [] =
  let
    leftCorner = head m
    (leftLine,s2) = splitAt (h-2) (tail m)
    (lastLine,s3) = splitAt w s2
    (rightLine,s4) = splitAt (h-2) s3
    firstLine = leftCorner : (reverse s4)
    mid = map (\(a,b) -> a:b:[]) $ zip leftLine (reverse rightLine)
  in firstLine:(mid ++ [lastLine]) 
matrixWrap h w m im =
  let
    leftCorner = head m
    (leftLine,s2) = splitAt (h-2) (tail m)
    (lastLine,s3) = splitAt w s2
    (rightLine,s4) = splitAt (h-2) s3
    firstLine = leftCorner : (reverse s4)
    mid =  map (\(a,b,l) -> a:(l ++ [b])) $ zip3 leftLine (reverse rightLine) im
  in firstLine:(mid ++ [lastLine]) 
  
matrixJoin :: Int -> Int -> [[Int]] -> [[Int]]
matrixJoin _ _ [] = []
matrixJoin 0 _ _ = []
matrixJoin _ 0 _ = []
matrixJoin h w (x:xs) =
  let
    im = matrixJoin (h-2) (w-2) xs
  in  matrixWrap h w x im
