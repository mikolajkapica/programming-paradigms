cutAndMend lst a b =
    aux lst 0 where
        aux [] _ = []
        aux (h:t) current
            | current >= a && current <= b = aux t (current + 1)
            | otherwise = h : aux t (current + 1)