import qualified Data.Set as Set
main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show (countOverlaps (filter (not.isDiagonal) measurements)) ++ "\n"
           ++ "Problem2: " ++ show (countOverlaps measurements) ++ "\n"
    where
        measurements = map splitNumbers (lines input) :: [[Int]]
        splitNumbers line = read num : if null rest then [] else splitNumbers rest
            where
                (num,rest) = span isNum (dropWhile (not.isNum) line)
                isNum c = elem c "0123456789"

countOverlaps measurements = length (snd (foldr countPts (Set.empty, Set.empty) measurements))
    where
        countPts line (allPts, dups) = (allPts', dups')
            where
                allPts' = Set.union allPts newPts
                dups' = Set.union dups (Set.intersection allPts newPts)
                newPts = Set.fromList (points line)

points line@(x1:y1:x2:y2:[]) = if isDiagonal line then zip xs ys else [(x,y) | x<-xs, y<-ys]
    where
        xs = listPts x1 x2
        ys = listPts y1 y2
        listPts p1 p2
            | p1 > p2 = reverse [p2..p1]
            | otherwise = [p1..p2]

isDiagonal (x1:y1:x2:y2:[]) = x1/=x2 && y1/=y2
