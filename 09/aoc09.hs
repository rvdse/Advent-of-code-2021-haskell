import Data.List
import qualified Data.Set as Set

main = interact solve

--solve input = "Problem 1: " ++ show (sum (map (\pt -> 1 + getHeight heightmap pt) (filter (isLowPoint heightmap) allPts))) ++ "\n"
solve input = "Problem 1: " ++ show (sum (map riskLevel allLowPts)) ++ "\n"
           ++ "Problem 2: " ++ show ((product.(take 3).reverse.sort) (map (getBasinSize heightmap) allLowPts)) ++ "\n"
    where
        riskLevel p = 1 + getHeight heightmap p
        allLowPts = filter (isLowPoint heightmap) allPts
        allPts = [(x, y) | y <- ys, x <- xs]
        xs = [0..(length (head heightmap) -1)]
        ys = [0..(length heightmap - 1)]
        heightmap = lines input

isLowPoint :: [String] -> (Int, Int) -> Bool
isLowPoint heightmap (x, y) = h < hN && h < hW && h < hE && h < hS
    where
        h = getHeight heightmap (x, y)
        hN = getHeight heightmap (x, y-1)
        hW = getHeight heightmap (x-1, y)
        hE = getHeight heightmap (x+1, y)
        hS = getHeight heightmap (x, y+1)

getHeight :: [String] -> (Int, Int) -> Int
getHeight heightmap (x, y)
    | x < 0 || x >= length (head heightmap) = 999
    | y < 0 || y >= length heightmap = 999
    | otherwise = read [((heightmap !! y) !! x)] :: Int

getBasinSize heightmap (x,y) = Set.size (getBasin heightmap (x,y))

getBasin heightmap (x,y)
    | getHeight heightmap (x,y) >= 9 = Set.empty
    | otherwise = Set.union (Set.singleton (x,y))
                 (Set.union (if getHeight heightmap (x, y-1) > getHeight heightmap (x,y) then getBasin heightmap (x, y-1) else Set.empty)
                 (Set.union (if getHeight heightmap (x-1, y) > getHeight heightmap (x,y) then getBasin heightmap (x-1, y) else Set.empty)
                 (Set.union (if getHeight heightmap (x+1, y) > getHeight heightmap (x,y) then getBasin heightmap (x+1, y) else Set.empty)
                            (if getHeight heightmap (x, y+1) > getHeight heightmap (x,y) then getBasin heightmap (x, y+1) else Set.empty))))
