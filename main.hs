import Control.Monad.State
import Data.List
import Data.Maybe

data Xro = X | O deriving (Show, Eq)

type Space = Maybe Xro
type Board = [[Space]]
type Position = (Int, Int)
data GameResult = Win Xro | Die deriving (Show)
type GameState = (Xro, Board)

gameInit :: GameState
gameInit = (X, replicate 3 (replicate 3 Nothing))

getBoard :: GameState -> Board
getBoard (_, x) = x

getSt :: Maybe Xro -> Char
getSt Nothing = ' '
getSt (Just X) = 'X'
getSt (Just O) = 'O'

boardDraw :: Board -> IO ()
boardDraw board = mapM_ putStrLn $ intersperse "-----" $ map showRow board

showRow :: [Space] -> String
showRow row = intersperse '|' $ map getSt row


makeMove :: GameState -> Position -> Maybe GameState
makeMove (player, board) position
    | isValidMove board position = Just (nextPlayer player, updateBoard board player position)
    | otherwise = Nothing

isValidMove :: Board -> Position -> Bool
isValidMove board (row, col) = isNothing $ board !! row !! col

updateBoard :: Board -> Xro -> Position -> Board
updateBoard board player (row, col) =
    let updatedRow = take col (board !! row) ++ [Just player] ++ drop (col+1) (board !! row)
    in take row board ++ [updatedRow] ++ drop (row+1) board

nextPlayer :: Xro -> Xro
nextPlayer X = O
nextPlayer O = X

checkResult :: GameState -> Maybe GameResult
checkResult (player, board)
    | any (all (== Just O)) rows                                       = Just (Win O)
    | any (all (== Just O)) cols                                       = Just (Win O)
    | all (\(i, j) -> board !! i !! j == Just O) [(0,0), (1,1), (2,2)] = Just (Win O)
    | all (\(i, j) -> board !! i !! j == Just O) [(0,2), (1,1), (2,0)] = Just (Win O)
    | any (all (== Just X)) rows                                       = Just (Win X)
    | any (all (== Just X)) cols                                       = Just (Win X)
    | all (\(i, j) -> board !! i !! j == Just X) [(0,0), (1,1), (2,2)] = Just (Win X)
    | all (\(i, j) -> board !! i !! j == Just X) [(0,2), (1,1), (2,0)] = Just (Win X)
    | all (all ((\i -> i == Just X || i == Just O))) board             = Just Die
    | otherwise                                                        = Nothing
    where
        rows = board
        cols = Data.List.transpose board

gameLoop :: GameState -> IO ()
gameLoop gameState = do
    let board = getBoard gameState
    let currentPlayer = fst gameState
    putStrLn $ "Current player: " ++ show currentPlayer
    boardDraw board
    putStrLn "Enter row number (1-3): "
    row <- readLn
    putStrLn "Enter column number (1-3): "
    col <- readLn
    let position = (row - 1, col - 1)
    case makeMove gameState position of
        Just newGameState -> do
            case checkResult newGameState of
                Just Die -> putStrLn $ "Draw!"
                Just result -> putStrLn $ "Player " ++ show currentPlayer ++ " wins!"
                Nothing -> gameLoop newGameState
        Nothing -> do
            putStrLn "Invalid move, try again."
            gameLoop gameState

getMode :: Int -> Bool
getMode x = if (x == 1) then (True) else (False)

-- X player
-- O computer
pcMove :: GameState -> (Int, Int)
pcMove (player, board)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,0), (1,1)] || all (\(i, j) -> board !! i !! j == Just O) [(0,0), (1,1)])) && (board !! 2 !! 2 == Nothing)   = (2, 2) -- \
    | ((all (\(i, j) -> board !! i !! j == Just X) [(1,1), (2,2)] || all (\(i, j) -> board !! i !! j == Just O) [(1,1), (2,2)])) && (board !! 0 !! 0 == Nothing)   = (0, 0)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(2,2), (0,0)] || all (\(i, j) -> board !! i !! j == Just O) [(2,2), (0,0)])) && (board !! 1 !! 1 == Nothing)   = (1, 1)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,2), (1,1)] || all (\(i, j) -> board !! i !! j == Just O) [(0,2), (1,1)])) && (board !! 2 !! 0 == Nothing)   = (2, 0) -- /
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,2), (2,0)] || all (\(i, j) -> board !! i !! j == Just O) [(0,2), (2,0)])) && (board !! 1 !! 1 == Nothing)   = (1, 1)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(2,0), (1,1)] || all (\(i, j) -> board !! i !! j == Just O) [(2,0), (1,1)])) && (board !! 0 !! 2 == Nothing)   = (0, 2)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,0), (1,0)] || all (\(i, j) -> board !! i !! j == Just O) [(0,0), (1,0)])) && (board !! 2 !! 0 == Nothing)   = (2, 0) -- (|  )
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,0), (2,0)] || all (\(i, j) -> board !! i !! j == Just O) [(0,0), (2,0)])) && (board !! 1 !! 0 == Nothing)   = (1, 0)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(1,0), (2,0)] || all (\(i, j) -> board !! i !! j == Just O) [(1,0), (2,0)])) && (board !! 0 !! 0 == Nothing)   = (0, 0)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,1), (1,1)] || all (\(i, j) -> board !! i !! j == Just O) [(0,1), (1,1)])) && (board !! 2 !! 1 == Nothing)   = (2, 1) -- ( | )
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,1), (2,1)] || all (\(i, j) -> board !! i !! j == Just O) [(0,1), (2,1)])) && (board !! 1 !! 1 == Nothing)   = (1, 1)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(1,1), (2,1)] || all (\(i, j) -> board !! i !! j == Just O) [(1,1), (2,1)])) && (board !! 0 !! 1 == Nothing)   = (0, 1)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,2), (1,2)] || all (\(i, j) -> board !! i !! j == Just O) [(0,2), (1,2)])) && (board !! 2 !! 2 == Nothing)   = (2, 2) -- (  |)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,2), (2,2)] || all (\(i, j) -> board !! i !! j == Just O) [(0,2), (2,2)])) && (board !! 1 !! 2 == Nothing)   = (1, 2)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(1,2), (2,2)] || all (\(i, j) -> board !! i !! j == Just O) [(1,2), (2,2)])) && (board !! 0 !! 2 == Nothing)   = (0, 2)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,0), (0,1)] || all (\(i, j) -> board !! i !! j == Just O) [(0,0), (0,1)])) && (board !! 0 !! 2 == Nothing)   = (0, 2) -- (^)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,0), (0,2)] || all (\(i, j) -> board !! i !! j == Just O) [(0,0), (0,2)])) && (board !! 0 !! 1 == Nothing)   = (0, 1)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(0,1), (0,2)] || all (\(i, j) -> board !! i !! j == Just O) [(0,1), (0,2)])) && (board !! 0 !! 0 == Nothing)   = (0, 0)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(1,0), (1,1)] || all (\(i, j) -> board !! i !! j == Just O) [(1,0), (1,1)])) && (board !! 1 !! 2 == Nothing)   = (1, 2) -- (-)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(1,0), (1,2)] || all (\(i, j) -> board !! i !! j == Just O) [(1,0), (1,2)])) && (board !! 1 !! 1 == Nothing)   = (1, 1)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(1,1), (1,2)] || all (\(i, j) -> board !! i !! j == Just O) [(1,1), (1,2)])) && (board !! 1 !! 0 == Nothing)   = (1, 0)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(2,0), (2,1)] || all (\(i, j) -> board !! i !! j == Just O) [(2,0), (2,1)])) && (board !! 2 !! 2 == Nothing)   = (2, 2) -- (_)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(2,0), (2,2)] || all (\(i, j) -> board !! i !! j == Just O) [(2,0), (2,2)])) && (board !! 2 !! 1 == Nothing)   = (2, 1)
    | ((all (\(i, j) -> board !! i !! j == Just X) [(2,1), (2,2)] || all (\(i, j) -> board !! i !! j == Just O) [(2,1), (2,2)])) && (board !! 2 !! 0 == Nothing)   = (2, 0)
    | otherwise  = if ( board !! 1 !! 1 == Nothing) then ((1, 1)) else (if ( board !! 0 !! 0 == Nothing) then ((0, 0)) else (if ( board !! 2 !! 2 == Nothing) then ((2, 2)) else (if ( board !! 0 !! 2 == Nothing) then ((0, 2)) else (if ( board !! 2 !! 0 == Nothing) then ((2, 0)) else (1, 2)))))

gameLoopPc :: GameState -> IO ()
gameLoopPc gameState = do
    let board = getBoard gameState
    let currentPlayer = fst gameState
    putStrLn $ "Your(" ++ show currentPlayer ++ ") move: "
    boardDraw board
    putStrLn "Enter row number (1-3): "
    row <- readLn
    putStrLn "Enter column number (1-3): "
    col <- readLn
    let position = (row - 1, col - 1)
    case makeMove gameState position of
        Just newGameState -> do
            case checkResult newGameState of
                Just Die -> putStrLn $ "Draw!"
                Just result -> putStrLn $ "Player " ++ show currentPlayer ++ " wins!"
                Nothing -> do
                    putStrLn $ "Computer make move!"
                    let positionPc = pcMove newGameState
                    case makeMove newGameState positionPc of
                        Just newGameState2 -> do
                            case checkResult newGameState2 of
                                Just Die -> putStrLn $ "Draw!"
                                Just result -> do
                                    boardDraw (snd newGameState2)
                                    putStrLn $ "Computer wins!"
                                Nothing -> gameLoopPc newGameState2
                        Nothing -> do
                            putStrLn "Error PC Move."
                            gameLoopPc newGameState
        Nothing -> do
            putStrLn "Invalid move, try again."
            gameLoopPc gameState

main :: IO ()
main = do
    putStrLn "Enter game mode (1 - Player vs Player. 2 - Player vs Computer): "
    mode <- readLn
    case getMode mode of
        True -> do
            gameLoop gameInit
        False -> do
            gameLoopPc gameInit
  
