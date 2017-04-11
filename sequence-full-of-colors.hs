data ColorSeq = ColorSeq Int Int Int Int Bool
  deriving (Show)


main :: IO()
main = do
  x <- getContents
  putStrLn $ unlines $ map (show .isOk . (fullseq (ColorSeq 0 0 0 0 True))) $ tail $ lines x
  
isOk :: ColorSeq -> Bool
isOk (ColorSeq _ _ _ _ ok) = ok

fullseq :: ColorSeq -> String -> ColorSeq

fullseq (ColorSeq r g y b False) _  = (ColorSeq r g y b False)
fullseq (ColorSeq r g y b ok) [] = (ColorSeq r g y b ok)
fullseq  (ColorSeq r g y b ok) ('R':xs)=
  if r>= g+1 then  (ColorSeq (r+1) g y b False) else fullseq (ColorSeq (r+1) g y b True) xs
fullseq (ColorSeq r g y b ok) ('G':xs)=
  if g>= r+1 then  (ColorSeq r (g+1) y b False) else fullseq (ColorSeq r (g+1) y b True) xs
fullseq (ColorSeq r g y b ok) ('Y':xs)=
  if y>= b+1 then  (ColorSeq r g (y+1) b False) else fullseq (ColorSeq r g (y+1) b True) xs
fullseq (ColorSeq r g y b ok) ('B':xs)=
  if b>= y+1 then  (ColorSeq r g y (b+1) False) else fullseq (ColorSeq r g y (b+1) True) xs
