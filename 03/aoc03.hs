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

solution2 measurements = toDecimal (oxygen (mostCommonBit) []) * toDecimal (oxygen (leastCommonBit) [])
    where
        oxygen bitSelector prefix
            | length (matches prefix) == 1 = head (matches prefix)
            | length prefix == length (head measurements) = prefix
            | otherwise = oxygen (bitSelector) (prefix ++ [bitSelector prefix])
        mostCommonBit prefix = head (drop (div (length (firstBitOfMatches prefix)) 2) (firstBitOfMatches prefix))
        leastCommonBit prefix = 1 - (mostCommonBit prefix)
        matches prefix = filter (prefix `isPrefixOf`) measurements
        firstBitOfMatches prefix = sort (map (head . (drop (length prefix))) (matches prefix))
        toDecimal bits = foldr (\x acc -> 2*acc + x) 0 (reverse bits)
