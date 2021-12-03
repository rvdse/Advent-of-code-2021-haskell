main = interact solve

solve input = "Problem 1: " ++ show (solution1 measurements) ++ "\n"
           ++ "Problem 2: " ++ show (solution2 measurements) ++ "\n"
    where
        measurements = map (parseLine . words) (lines input)
        parseLine [dir, dist] = (dir, read dist::Int)

solution1 measurements = hzPos * depth
    where
        (hzPos, depth) = foldr move (0,0) measurements
        move (dir, d) (x,y)
            | dir == "forward" = (x+d, y  )
            | dir == "up" =      (x  , y-d)
            | dir == "down" =    (x  , y+d)

solution2 measurements = hzPos * depth
    where
        (hzPos, depth, aim) = foldr move (0,0,0) (reverse measurements)
        move (dir, d) (x,y,a)
            | dir == "forward" = (x+d, y+d*a, a  )
            | dir == "up" =      (x  , y    , a-d)
            | dir == "down" =    (x  , y    , a+d)
