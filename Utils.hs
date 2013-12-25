module Utils where

{--
power :: [a] -> [[a]]
power []     = [[]]
power (x:xs) = power xs ++ map (x:) (power xs)
--}
power :: [a] -> [[a]]
power []     = [[]]
power (x:xs) = xss /\/ map (x:) xss
      where
      xss = power xs

      (/\/) :: [a] -> [a] -> [a]
      []     /\/ ys = ys
      (x:xs) /\/ ys = x : (ys /\/ xs)