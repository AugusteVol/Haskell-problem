main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStr (solve contents)

solve :: String -> String
solve input = unlines [ show (findRoot n p 1 p) | (n:p:_) <- chunks 2 (map read (lines input)) ]
  where
    chunks _ [] = []
    chunks k xs = let (h, t) = splitAt k xs in h : chunks k t

power :: Integer -> Integer -> Integer
power base exp = base ^ exp

findRoot :: Integer -> Integer -> Integer -> Integer -> Integer
findRoot n p low high
    | low > high = low 
    | midPower < p = findRoot n p (mid + 1) high
    | otherwise = findRoot n p low (mid - 1)
    where
      mid = (low + high) `div` 2 
      midPower = power mid n
