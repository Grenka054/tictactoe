module Main where
import Tictactoe (startVsCPU, startPvP)
import Control.Monad
import System.IO ( hFlush, stdout )
import Data.Time.Clock ( getCurrentTime, UTCTime(utctDayTime), DiffTime )
import System.IO.Unsafe ( unsafePerformIO )
import System.Random
import System.Random (mkStdGen)

getTime :: IO Integer
getTime = do
    time <- getCurrentTime >>= return . utctDayTime
    return $ diffTimeToSeconds time

diffTimeToSeconds :: DiffTime -> Integer
diffTimeToSeconds = floor . toRational
main = main' [mkStdGen $ fromIntegral $ unsafePerformIO getTime]

main' gen = do
    putStr "Tic Tac Toe\n 1) vs CPU: cpu\n 2) 2 players: 2p\n exit\n> "
    hFlush stdout
    ans <- getLine
    if ans == "1" || ans == "cpu" then do
        startVsCPU gen
        let (a,b) = randomR (0,9) (mkStdGen $ length gen) :: (Int, StdGen)
        main' (gen ++ [b])
    else when (ans == "2" || ans == "2p") $ do
        startPvP
        main' gen