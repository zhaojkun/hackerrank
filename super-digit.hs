import Data.Char
main :: IO()
main = do
  [x,ks] <- words <$> getLine
  let r = superDigitString 0 x
  let m = superDigitMul r $ read ks
  putStrLn $ show m

superDigitMul :: Int -> Int -> Int
superDigitMul x 0 = 0
superDigitMul x n =
  let
    y = superDigitMul x (n `div` 2)
    z = superDigit (y+y)
  in if (n `mod` 2)==1 then superDigit (z+x) else z
  
superDigitString :: Int -> String -> Int
superDigitString n [] = superDigit n
superDigitString n (x:xs) =
  let
    m = digitToInt x
    a = superDigit (n+m)
  in superDigitString a xs

superDigit :: Int -> Int
superDigit n = if n < 10 then n else superDigit . sum $ digs n

digs :: Int -> [Int]
digs 0 = []
digs n =  n `mod` 10 : digs (n `div` 10)

