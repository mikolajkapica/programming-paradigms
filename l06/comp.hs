composites n = get n (filter (\n -> length [x | x <- [2..n-1], n `mod` x == 0] == 0) [3..])