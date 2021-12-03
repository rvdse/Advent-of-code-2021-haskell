-- The program expects the input on stdin, ie
-- $ ./solve < input

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show (solve1 measurements) ++ "\n"
           ++ "Problem 2: " ++ show (solve2 measurements) ++ "\n"
           where measurements = map read (lines input) :: [Int]

solve1 = numIncrements
solve2 = numIncrements . slidingWindow

slidingWindow :: [Int] -> [Int]
slidingWindow (x:y:z:zs) = (x+y+z):slidingWindow (y:z:zs)
slidingWindow _ = []

numIncrements :: [Int] -> Int
numIncrements (x:y:ys)
    | x < y = 1 + numIncrements (y:ys)
    | otherwise = numIncrements (y:ys)
numIncrements _ = 0

