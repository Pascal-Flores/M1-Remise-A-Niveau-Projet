data Player = PlayerOne | PlayerTwo | Terrain deriving (Enum)
data Pawn = Pawn { player :: Player, level :: Int}
type Position = (Int, Int)
data Board = Board {size :: (Int, Int), pawns :: [[Pawn]]}

createBoard :: (Int, Int) -> Board
createBoard size =
    Board size (replicate (snd size) (replicate (fst size) (Pawn Terrain 1)))

renderBoard :: Board -> IO()
renderBoard board = do
    let renderPawn pawn = if level pawn == 0 then " " else show $ level pawn
    let renderedRows = map (concatMap renderPawn) $ pawns board
    putStr $ unlines renderedRows

main = do
    let b = createBoard (8,8)
    renderBoard b