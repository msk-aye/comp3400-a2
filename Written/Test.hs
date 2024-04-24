module Test where

-- Example
lrnorm :: [Float] -> Float
lrnorm xs = sqrt (sum (map (** 2) xs))

trNorm :: Float -> [Float] -> Float
trNorm total [] = sqrt total
trNorm total (x : xs) = trNorm (total + x ** 2) xs

-- Q2
lrMean :: [Float] -> Float
lrMean [] = 0
lrMean (x:xs) = (1 / fromIntegral (length (x:xs))) * sum (x:xs)

-- Q4
trMean :: Float -> Float -> [Float] -> Float
trMean n mean [] = mean
trMean n mean (x:xs) = trMean (n + 1) ((n * mean + x) / (n + 1)) xs

-- Q5
mean :: [Float] -> Float
mean = trMean 0 0



-- Junk
data Tree a = Tree a [Tree a]
  deriving (Eq, Show)

step :: Tree a -> [Tree a]
step (Tree _ []) = []
step (Tree _ (t : ts)) = t : ts

thing :: [Tree a] -> [Tree a]
thing = concatMap step

smallest :: [Tree a] -> Maybe (Tree a)
smallest [] = Nothing
smallest [Tree a []] = Just (Tree a [])
smallest (t : ts) = smallest $ thing (t : ts)

height :: Tree a -> Int
height (Tree _ []) = 1
height (Tree _ ts) = 1 + maximum (map height ts)