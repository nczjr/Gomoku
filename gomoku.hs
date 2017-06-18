import Data.List
import System.Random
import System.IO.Unsafe

data Board = Board {size :: Int, cells :: [[Point]]}

data Point = Point {x :: Int, y :: Int, color :: Color}

data Color = Black | White | Empty deriving Eq


instance Eq Point where
    (Point _ _ color1) == (Point _ _ color2) = color1 == color2

instance Show Board where
    show (Board _ rows) = intercalate "\n" $ map show rows

instance Show Point where
    show (Point x y color) = show color

instance Show Color where
    show Empty = " _ "
    show Black = " o "
    show White = " x "

makeColumns :: Int -> Int -> [Point]-> [Point]
makeColumns x y list
    | y == -1 = list
    | otherwise = makeColumns x (y-1) (b:list)
    where b = Point x y Empty

makeBoard :: Int -> Int-> [[Point]] -> [[Point]]
makeBoard x y list
    | x == -1 = list
    | otherwise = makeBoard (x-1) y (b:list)
    where b = makeColumns x y []

board :: Board
board = Board 19 (makeBoard 18 18 [])

insertFigure :: Board -> Int -> Int -> Color -> Board
insertFigure board x y figure
    | x > size board || y > size board || x < 0 || y < 0 = board
    | color (cells board !! x !! y) == Empty = Board 19 (insertF board x y figure)
    | color (cells board !! x !! y) == figure = board


insertF :: Board -> Int -> Int -> Color -> [[Point]]
insertF board x y figure =
     let (a,b) = splitAt x (cells board)
        in let d = [addPoint x y figure (head b)]
            in a ++ (d) ++ (tail b)


addPoint :: Int -> Int -> Color -> [Point] -> [Point]
addPoint x y figure list =
	let (a,b) = splitAt y list in (a ++ [Point x y figure] ++ (tail b))

switchPlayer :: Color -> Color
switchPlayer color
  | color == Black = White
  | color == White = Black


getRandomPosition :: Board -> Color -> Board
getRandomPosition board player = checkIfMovePossible board player (unsafePerformIO(getStdRandom (randomR(0,18)))) (unsafePerformIO (getStdRandom ( randomR(0,18))))

checkIfMovePossible :: Board -> Color -> Int -> Int -> Board
checkIfMovePossible board player x y
  | color (cells board !! x !! y) == Empty = Board 19 (insertF board x y player)
  | otherwise = getRandomPosition board player


newBoard1 = getRandomPosition board Black
newBoard2 = getRandomPosition newBoard1 Black
newBoard3 = getRandomPosition newBoard2 Black
newBoard4 = getRandomPosition newBoard3 Black
newBoard5 = getRandomPosition newBoard4 Black
newBoard6 = getRandomPosition newBoard5 Black

-- newBoard1 = Board 19 (insertF board 1 1 Black)
-- newBoard2 = Board 19 (insertF newBoard1 2 2 Black)
-- newBoard3 = Board 19 (insertF newBoard2 3 3 Black)
-- newBoard4 = Board 19 (insertF newBoard3 4 4 Black)
-- newBoard5 = Board 19 (insertF newBoard4 5 11 Black)


checkForWinInDiagonals :: [[Point]] -> Int -> Point -> Bool
checkForWinInDiagonals cells number point
  | number>= 37 || number < 0 = False
  | checkIf5(getIndexes point cells number) == True = True
  | otherwise = checkForWinInDiagonals cells num point
    where num = number+1


checkForWinInCols :: Board -> Int -> Point -> Bool
checkForWinInCols board number point = checkForWinInRows (Board 19 (rotateBoard $  getCells (board))) number point


checkForWinInRows :: Board -> Int -> Point -> Bool
checkForWinInRows (Board size cells) number point
  | number >= size || number < 0 = False
  | checkIf5(getIndexes point cells number) == True = True
  | otherwise = checkForWinInRows (Board size cells) num point
    where num = number+1


checkIfWinner :: [Int] -> Bool
checkIfWinner list
  | (maximum list - (minimum list)) == 4 = True
  | otherwise = False

checkIfWinner1 :: [Int] -> Int -> Bool
checkIfWinner1 (x:[]) sum
  | sum ==4 = True
  | otherwise = False
checkIfWinner1 (x:y:xs) sum
  | sum == 4 = True
  | y == x+1 = checkIfWinner1 (y:xs) (sum+1)
  | otherwise = checkIfWinner1 (y:xs) 0


getIndexes :: Point -> [[Point]] -> Int -> [Int]
getIndexes (Point x y color) line number = elemIndices (Point 0 0 color) (line !! number)



checkIf5 :: [Int] -> Bool
checkIf5 list
  | (length list) == 5 = checkIfWinner list
  | (length list) >5 = checkIfWinner1 list 0
  | otherwise = False

rotateBoard :: [[Point]] -> [[Point]]
rotateBoard = transpose . reverse

getCells :: Board -> [[Point]]
getCells (Board size cells) = cells


diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]


isGameWon :: Board -> Color -> Bool
isGameWon board player = do
  let a = checkForWinInRows board 0 (Point 0 0 player)
  let b = checkForWinInCols board 0 (Point 0 0 player)
  let c = checkForWinInDiagonals (diagonals $ getCells board) 0 (Point 0 0 player)
  let d = checkForWinInDiagonals (diagonals $ rotateBoard (getCells board)) 0 (Point 0 0 player)
  if (a || b || c || d) then do True
    else do False


check = do
  let player = Black
  let f = isGameWon newBoard5 player
  f



playersMode color board = do
  let player = color
  putStrLn ""
  putStrLn (  show player ++ " move")
  putStrLn "Choose X position "
  x <-getLine
  putStrLn "Choose Y position "
  y <-getLine
  let newBoard = Board 19 (insertF board ((read x :: Int)-1) ((read y :: Int) -1) player)
  putStrLn $ show newBoard
  if (isGameWon newBoard player) then do putStrLn $ "Player " ++ show player ++ " won"
    else do
      let newPlayer = switchPlayer player
      playersMode newPlayer newBoard


playerComputerMode color board = do
  let player = color
  putStrLn ""
  putStrLn (  show player ++ " move")
  if (player == White) then do
    putStrLn "Choose X position "
    x <-getLine
    putStrLn "Choose Y position "
    y <-getLine
    let newBoard = Board 19 (insertF board ((read x :: Int)-1) ((read y :: Int) -1) player)
    putStrLn $ show newBoard
    if (isGameWon newBoard player) then do putStrLn $ "Player " ++ show player ++ " won"
      else do
        let newPlayer = switchPlayer player
        playerComputerMode newPlayer newBoard
  else do
    let newBoard = getRandomPosition board player
    putStrLn $ show newBoard
    if (isGameWon newBoard player) then do putStrLn $ "Player " ++ show player ++ " won"
      else do
        let newPlayer = switchPlayer player
        playerComputerMode newPlayer newBoard


computersMode player board = do
  putStrLn $ show player ++ " move"
  let newBoard = getRandomPosition board player
  putStrLn $ show newBoard
  if (isGameWon newBoard player) then do putStrLn $ "Player " ++ show player ++ " won"
    else do
      let newPlayer = switchPlayer player
      computersMode newPlayer newBoard
main = do
  putStr "Choose the mode : \n 1) Player vs player \n 2) Player vs Computer \n 3) Computer vs Computer \n"
  line <- getLine
  if (line == "1") then do playersMode White board
    else if (line == "2") then do playerComputerMode White board
      else do computersMode White board
