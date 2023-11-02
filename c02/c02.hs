even' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)

-- 2
fib :: (Eq t, Num t, Num a) => t -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibTail :: (Eq t1, Num t1, Num t2) => t1 -> t2
fibTail n = aux n 0 1
    where aux 0 a _ = a
          aux n a b = aux (n-1) b (a+b)
        
-- 3
root3 :: Double -> Double
root3 a | a > 1 = aux a (a/3)
        | otherwise = aux a a
        where aux a x | abs (x^3 - a) < 1/10^15 = x
                      | otherwise = aux a (x - (x^3 - a) / (3 * x^2))



-- 4
getZeroLTuples :: [(a, b)] -> a
getZeroLTuples [(_, _), (x, _)] = x

getZeroList :: [a] -> a
getZeroList [_, _, x, _, _] = x

-- 5
initSegment :: Eq a => ([a], [a]) -> Bool
initSegment ([], _) = True
initSegment (x:xs, y:ys) = x == y && initSegment (xs, ys)

-- 6
replaceNth :: [a] -> Int -> a -> [a]
replaceNth lst 0 x = x : tail lst
replaceNth lst n x = head lst : replaceNth (tail lst) (n-1) x

-- 7
remove :: [a] -> Int -> [a]
remove [] _ = []
remove lst 0 = tail lst
remove lst n = head lst : remove (tail lst) (n-1)


