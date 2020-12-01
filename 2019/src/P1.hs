module P1 where

main :: IO ()
main = do
  s <- readFile "p1.txt"
  let s' = lines s
  -- read :: string -> int
  print $ sum $ map sum $ map math' (map read s')
  print $ sum $ map (sum . math' . read) s'


math :: Int -> Int
math x = div x 3 - 2

math' :: Int -> [Int]
math' x =
  if y <= 0 then []
  else
    y : math' y
    where y = math x

  

  
