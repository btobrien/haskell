
import Data.Maybe
import Data.List.Split
import System.IO
import System.Environment
import System.Exit

type Move = (Int,Int)
type Disk = Int
type Stack = [Disk]
data Board = Board { left :: Stack, center :: Stack, right :: Stack } deriving (Eq, Show)

update :: Int -> (Stack -> Stack) -> Board -> Maybe Board
update 0 modify board = Just $ board { left = modify (left board) }
update 1 modify board = Just $ board { center = modify (center board) }
update 2 modify board = Just $ board { right = modify (right board) }
update _ _ _ = Nothing

top :: Int -> Board -> Maybe Disk
top 0 = safehead.left
top 1 = safehead.center
top 2 = safehead.right
top _ = const Nothing
safehead = listToMaybe . take 1

push :: Int -> Disk -> Board -> Maybe Board
push index disk board = if disk < fromMaybe maxBound (top index board)
    then update index (disk:) board
    else Nothing

pop :: Int -> Board -> Maybe (Disk,Board)
pop index board = (,) <$> top index board <*> update index (drop 1) board

trymove :: (Int,Int) -> Board -> Maybe Board
trymove (from,to) board = pop from board >>= uncurry (push to)

move :: (Int,Int) -> Board -> Board
move mv board = fromMaybe board $ trymove mv board

start :: Int -> Board
start n = Board [1..n] [] []

parsemoves :: String -> [Move]
parsemoves =
    map ((,) <$> head <*> head.tail) .  chunksOf 2 .
    map (\c -> fromEnum c - fromEnum 'j')

scanMovesFrom :: Board -> [Move] -> [Board]
scanMovesFrom init = scanl (flip move) init

won :: Board -> Bool
won = (&&) <$> null.left <*> null.center

display :: Board -> IO ()
display board = do
    print board
    if won board
        then exitWith ExitSuccess 
        else return ()

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    size <- read . fromMaybe "5" . listToMaybe <$> getArgs
    mapM_ display . scanMovesFrom (start size) . parsemoves =<< getContents
