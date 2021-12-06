main = interact solve

solve input = "Problem 1: " ++ (show.sum.cycle) 80 ++ "\n"
           ++ "Problem 2: " ++ (show.sum.cycle) 256 ++ "\n"
    where
        -- cycle n returns a list where the element at index i
        -- is the number of fish at age i after n generations.
        cycle 0 = map (\age->length (filter (\c->c==age) input)) ['0'..'8']
        cycle n = zipWith (+) (prevTail++[0]) ([0,0,0,0,0,0,prevHead,0,prevHead])
            where (prevHead:prevTail) = cycle (n-1)
