import Data.List
import qualified Data.Set as Set

main = interact solve

solve input = "Problem 1: " ++ show (sum (map riskLevel lowPts)) ++ "\n"
           ++ "Problem 2: " ++ show ((product.(take 3).reverse.sort) (map ((Set.size).(getBasin heightmap)) lowPts)) ++ "\n"
    where
        riskLevel p = 1 + getHeight heightmap p
        lowPts = filter (isLowPoint heightmap) allPts
        allPts = [(x, y) | y <- [0..length heightmap-1], x <- [0..length (head heightmap)-1]]
        heightmap = lines input

within heightmap (x, y) = x>=0 && x<length (head heightmap) && y>=0 && y<length heightmap
getHeight heightmap (x, y) = read [((heightmap !! y) !! x)] :: Int

isLowPoint heightmap (x, y) = all higher surroundingPts
    where
        surroundingPts = filter (within heightmap) [(x, y-1), (x-1, y), (x+1, y), (x, y+1)]
        higher (x', y') = getHeight heightmap (x', y') > getHeight heightmap (x, y)


getBasin heightmap (x,y) = Set.union (Set.singleton (x,y))
         (Set.union (if relevant (x, y-1) then getBasin heightmap (x, y-1) else Set.empty)
         (Set.union (if relevant (x-1, y) then getBasin heightmap (x-1, y) else Set.empty)
         (Set.union (if relevant (x+1, y) then getBasin heightmap (x+1, y) else Set.empty)
                    (if relevant (x, y+1) then getBasin heightmap (x, y+1) else Set.empty))))
    where
        relevant (x', y') = valid (x', y') && higher (x', y') && (not.tooHigh) (x', y')
        valid (x', y') = within heightmap (x', y')
        higher (x', y') = getHeight heightmap (x', y') > getHeight heightmap (x, y)
        tooHigh (x', y') = getHeight heightmap (x', y') >= 9
