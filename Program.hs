main :: IO ()
main = do
    -- open and read "input.txt"
    contents <- readFile "input.txt"
    -- process text and print to console
    putStr (solve contents)

-- processes the input text line by line
solve :: String -> String
solve input = unlines [ show (findRoot n p 1 p) | (n:p:_) <- chunks 2 (map read (lines input)) ]
  where
    chunks _ [] = []
    chunks k xs = let (h, t) = splitAt k xs in h : chunks k t

-- power func
power :: Integer -> Integer -> Integer
power base exp = base ^ exp

-- root func
findRoot :: Integer -> Integer -> Integer -> Integer -> Integer
findRoot n p low high --2 16 1 16
    | low > high = low 
    | midPower < p = findRoot n p (mid + 1) high
    | otherwise = findRoot n p low (mid - 1)
    where -- is esmes taisykles KAS yra kas
      mid = (low + high) `div` 2 --div = floor. 7/2 = 3
      midPower = power mid n