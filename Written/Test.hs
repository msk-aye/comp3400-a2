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