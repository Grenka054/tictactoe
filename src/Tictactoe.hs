module Tictactoe where
import Data.Char(digitToInt)
import Control.Monad.State
import System.Random
import Data.Time.Clock
data Player = X | O deriving(Eq)
data Cell = Empty | Mark Player deriving(Eq)
data Field = Field [Cell] deriving(Eq)
data GameState = GameState Player Field deriving(Eq)

instance Show Player where
    show(X) = "X"
    show(O) = "O"

instance Show GameState where
    show (GameState player field) = "Turn: " ++ show (player) ++ show (field)

instance Show Cell where
    show (Empty) = " "
    show (Mark player) = show (player)
instance Show Field where
    show (Field list) = "\n " ++ show(list!!0) ++ " | " ++ show(list!!1) ++ " | " ++ show(list!!2) ++ "\n"
                        ++ replicate 11 '-' ++ "\n "
                        ++ show(list!!3) ++ " | " ++ show(list!!4) ++ " | " ++ show(list!!5) ++ "\n"
                        ++ replicate 11 '-' ++ "\n "
                        ++ show(list!!6) ++ " | " ++ show(list!!7) ++ " | " ++ show(list!!8)

initGame :: GameState
initGame = GameState X (Field (take 9 $ repeat Empty))

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

letterToInt :: Num a => Char -> a
letterToInt 'A' = 0
letterToInt 'B' = 1
letterToInt 'C' = 2
letterToInt '1' = 1
letterToInt '2' = 2
letterToInt '3' = 3
letterToInt _ = error "wrong coords"
--randomR (a,b)
rand :: [Int] -> IO Int
rand xs = getStdRandom (randomR (0, (length xs -1)))
countCell :: Num a => [Char] -> a
countCell str = letterToInt (str !! 0) * 3 + letterToInt (str !! 1) - 1

changeField :: (Ord t, Num t) => [Cell] -> t -> Player -> [Cell]
changeField [] _ _ = []
changeField (f:fs) ind player   | ind > 0 = f : changeField fs (ind - 1) player
                                | f == Empty = Mark player : fs
                                | otherwise = f:fs
makeTurn :: String -> GameState -> GameState
makeTurn str (GameState player (Field field))  | cells /= field = GameState (nextPlayer player) (Field cells)
                                                | otherwise = GameState player (Field field) where
    cells = changeField field (countCell str) player

endGame :: GameState -> Bool
endGame (GameState pl (Field l))    | l!!0 == l!!1 && l!!1 == l!!2 && l!!0 /= Empty = True
                                    | l!!3 == l!!4 && l!!4 == l!!5 && l!!3 /= Empty = True
                                    | l!!6 == l!!7 && l!!7 == l!!8 && l!!6 /= Empty = True
                                    | l!!0 == l!!4 && l!!4 == l!!8 && l!!0 /= Empty = True
                                    | l!!2 == l!!4 && l!!4 == l!!6 && l!!2 /= Empty = True
                                    | l!!0 == l!!3 && l!!3 == l!!6 && l!!0 /= Empty = True
                                    | l!!1 == l!!4 && l!!4 == l!!7 && l!!1 /= Empty = True
                                    | l!!2 == l!!5 && l!!5 == l!!8 && l!!2 /= Empty = True
                                    | otherwise = Empty `notElem` l
getPlayer :: GameState -> Player
getPlayer (GameState pl _) = pl

gameLoopPvP :: GameState -> IO ()
gameLoopPvP game = do
    coord <- getLine
    let player = show $ getPlayer game
        newt = makeTurn coord game
    if newt == game then putStrLn "You can't!" else putStrLn ""
    print newt
    if endGame newt then putStrLn (player ++ " Win!") else gameLoopPvP newt

getRand xs = randomR (0, 8) (mkStdGen 1)

makeTurnCPU ind (GameState player (Field field))  | cells /= field = GameState (nextPlayer player) (Field cells)
                                                | otherwise = GameState player (Field field) where
    cells = changeField field ind player
diffTimeToSeconds :: DiffTime -> Integer
diffTimeToSeconds = floor . toRational
--test2 :: Integer
test2 = do 
    a <- liftIO test
    return 1

test :: IO Integer
test = do
    time <- getCurrentTime >>= return . utctDayTime
    let a = diffTimeToSeconds time
    return a
--getPOSIXTime
genCPU :: GameState -> StdGen -> GameState
genCPU gs generR = do
    let (ranNum, generNew) = randomR(0,8) generR :: (Int, StdGen)
        newt2 = makeTurnCPU ranNum gs
    if newt2 == gs then genCPU gs generNew
    else newt2

--getFromPlayer :: GameState -> IO GameState
getFromPlayer :: GameState -> IO (GameState, String)
getFromPlayer game = do
    coord <- getLine
    let newt = makeTurn coord game
    if newt == game then do
        putStrLn "You can't!"
        getFromPlayer game
    else do
        return (newt, coord)

gameLoop :: GameState -> IO ()
gameLoop game = do
    let player = show $ getPlayer game
    (newt, coord) <- liftIO $ getFromPlayer game
    let generR = mkStdGen $ countCell coord
        newt = makeTurn coord game
    print newt
    if endGame newt then
        putStrLn "You Win!" 
    else do
        let newt2 = genCPU newt generR
        print newt2
        gameLoop newt2
    return()

start :: IO ()
start = do
    let game = initGame
    gameLoop game
    return()