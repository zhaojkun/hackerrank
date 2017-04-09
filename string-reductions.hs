main :: IO()
main = do
     str <- getLine
     let res = reduceStr str
     putStrLn res


reduceStr :: String -> String
reduceStr [] = []
reduceStr (x:xs) = x: reduceStr (filter (/= x) xs)
