import Data.List
main = interact solve

solve input = "Problem 1: " ++ show (solution1 measurements) ++ "\n"
           ++ "Problem 2: " ++ show (solution2 measurements) ++ "\n"
    where
        measurements = map parseLine (lines input)
        parseLine [] = []
        parseLine (x:xs) = (if x=='1' then 1 else 0):(parseLine xs)

solution1 measurements = gamma*epsilon
    where
        (gamma,epsilon) = foldl ratios (0,0) bitSums
        ratios (x,y) bitSum
            | 2*bitSum >= length measurements = (2*x+1,2*y  )
            | otherwise                       = (2*x  ,2*y+1)
        bitSums = foldr (zipWith (+)) accInit measurements
        accInit = (take.length.head) measurements (repeat 0)

solution2 m = (getRating m mostCommonBit) * (getRating m leastCommonBit)
    where
        mostCommonBit xs = if 2*sum xs >= length xs then 1 else 0
        leastCommonBit xs = 1 - (mostCommonBit xs)

getRating measurements searchStrategy = toDecimal (getRatingHelper [] searchStrategy)
    where
        getRatingHelper prefix searchStrategy =
            if length matches == 1
            then head matches
            else getRatingHelper (prefix ++ [searchStrategy nextColumn]) searchStrategy
                where
                    matches = filter (prefix `isPrefixOf`) measurements
                    nextColumn = map (head.(drop (length prefix))) matches
        toDecimal bits = foldr (\x acc -> 2*acc + x) 0 (reverse bits)
