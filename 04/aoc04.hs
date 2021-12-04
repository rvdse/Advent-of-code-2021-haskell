import Data.List
main = interact solve

solve input = solution drawing boards ++ "\n"
    where
        drawing = parseDraw ((head.lines) input) :: [Int]
        boards = (parseBoards.tail.lines) input :: [[Int]]

parseDraw line
    | elem ',' line = read draw:parseDraw (tail rest)
    | otherwise = read line:[]
    where (draw, rest) = break (\c -> c==',') line

parseBoards (_:a:b:c:d:e:bs) = [(map read (words a ++ words b ++ words c ++ words d ++ words e))] ++ parseBoards bs
parseBoards _ = []

solution drawing boards = "Quickest board has score " ++ show (chooseBoard drawing boards minimum)
                     ++ "\nSlowest board has score " ++ show (chooseBoard drawing boards maximum)

chooseBoard drawing boards strategy = sum remainingNumbers * last drawnNumbers
    where
        drawnNumbers = take (chosenFinishTurn+1) drawing
        remainingNumbers = boards !! (chosenBoard-1) \\ drawnNumbers
        chosenBoard = 1 + elemIndex' chosenFinishTurn finishTurnByBoard
        chosenFinishTurn = strategy finishTurnByBoard 
        finishTurnByBoard = map finishTurn boards
        finishTurn board = minimum (map finishTurnRow (boardRows board))
        finishTurnRow row = maximum (remapToDrawOrder drawing row)
        boardRows board = map (map ((!!) board)) rowIndices
            where rowIndices = [[0..4], [5..9], [10..14], [15..19], [20..24], [0,5..20], [1,6..21], [2,7..22], [3,8..23], [4,9..24]]
        remapToDrawOrder drawing row = map drawOrder row
            where drawOrder n = elemIndex' n drawing

elemIndex' i xs = case elemIndex i xs of
    Just x -> x
    Nothing -> 999
