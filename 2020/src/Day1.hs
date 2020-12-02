module Day1 where

p1 :: IO ()
p1 = print =<< go . parse <$> readFile "input/day1.txt" where
  parse = map read . lines
  go ns = head [ x * y | x <- ns, y <- ns, x + y == 2020 ]

p2 :: IO ()
p2 = print =<< go . parse <$> readFile "input/day1.txt" where
  parse = map read . lines
  go ns = head [ x * y * z | x <- ns, y <- ns, z <- ns, x + y + z == 2020 ]
