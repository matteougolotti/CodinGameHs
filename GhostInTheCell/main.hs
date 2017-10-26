import System.IO
import Data.Map as Map
import Data.List as List
import Prelude

(|>) x f = f x

data Factory = Factory {
    factoryId :: Int,
    factoryOwner :: Int,
    factoryCyborgs :: Int,
    factoryProduction :: Int
} deriving (Show)

data Troop = Troop {
    troopId :: Int,
    troopOwner :: Int,
    troopSrc :: Int,
    troopDst :: Int,
    troopCyborgs :: Int,
    troopTurnsLeft :: Int
} deriving (Show)

data MoveType = MOVE | WAIT
  deriving (Show)

data Move = Move {
    move :: MoveType,
    moveSrc :: Int,
    moveDst :: Int,
    moveCyborgs :: Int
}

instance Show Move where
    show m = case move m of 
        MOVE -> "MOVE " ++ show (moveSrc m) ++ " " ++ show (moveDst m) ++ " " ++ show (moveCyborgs m)
        WAIT -> "WAIT"

bestMove :: Map Int (Map Int Int) -> [Factory] -> [Troop] -> [Move] -> Move
bestMove _ _ _ [] = Move WAIT 0 0 0
bestMove graph factories troops moves =
    List.minimumBy (\ m1 m2 -> if factoryCyborgs (factories !! moveDst m1) > factoryCyborgs (factories !! moveDst m2) then GT else LT) moves

neighbors :: Map Int (Map Int Int) -> Int -> [Int]
neighbors graph src = keys (graph ! src)

generateMovesFromSrc :: [Factory] -> [Troop] -> Int -> [Int] -> [Move]
generateMovesFromSrc factories troops src = 
    Prelude.foldl (\ moves dst -> (Move MOVE src dst 1 : moves)) []

generateMoves :: Map Int (Map Int Int) -> [Factory] -> [Troop] -> [Move]
generateMoves graph factories troops = do
    let validSrcs = Prelude.filter (\ f -> factoryOwner f == 1 && factoryCyborgs f > 1) factories
    let srcIds = Prelude.map factoryId validSrcs
    Prelude.foldl (\ moves src ->
        generateMovesFromSrc factories troops src (neighbors graph src) ++ moves) [] srcIds

-- TODO fix this function
graphDoubleLink :: Map Int (Map Int Int) -> Int -> Int -> Int -> Map Int (Map Int Int)
graphDoubleLink graph s d v = do
    let g = Map.insert s (Map.insert d v (graph ! s)) graph  -- TODO Error inserting value in missing sub-graph
    Map.insert d (Map.insert s v (graph ! s)) g  -- TODO Handle cases of empty graph vs initialized graph

initGraph :: Int -> Map Int (Map Int Int) -> IO (Map Int (Map Int Int))
initGraph 0 m = return m
initGraph n m = do
    linkStr <- getLine
    let link = linkStr |> words |> Prelude.map read :: [Int]
    if member (head link) m
    then initGraph (n - 1) (graphDoubleLink m (head link) (link!!1) (link!!2))
    else initGraph (n - 1) (graphDoubleLink empty (head link) (link!!1) (link!!2))
    -- then initGraph (n - 1) (Map.insert (head link) (Map.insert (link !! 1) (link !! 2) (m ! head link)) m)
    --else initGraph (n - 1) (Map.insert (head link) (Map.insert (link !! 1) (link !! 2) empty) m)

readEntities :: [Factory] -> [Troop] -> Int -> IO ([Factory], [Troop])
readEntities factories troops 0 = return (factories, troops)
readEntities factories troops count = do
    entityStr <- getLine
    let inputs = entityStr |> words
    let entityId = read (head inputs) :: Int
    let arg1 = read (inputs!!2) :: Int
    let arg2 = read (inputs!!3) :: Int
    let arg3 = read (inputs!!4) :: Int
    let arg4 = read (inputs!!5) :: Int
    let arg5 = read (inputs!!6) :: Int
    if inputs!!1 == "FACTORY"
    then readEntities (Factory entityId arg1 arg2 arg3 : factories) troops (count - 1)
    else readEntities factories (Troop entityId arg1 arg2 arg3 arg4 arg5 : troops) (count - 1)

gameLoop :: Map Int (Map Int Int) -> IO ()
gameLoop graph = do
    entityCountStr <- getLine
    let entityCount = read entityCountStr :: Int
    (factories, troops) <- readEntities [] [] entityCount
    let moves = generateMoves graph factories troops
    print (bestMove graph factories troops moves)
    gameLoop graph

main :: IO ()
main= do
    hSetBuffering stdout NoBuffering
    factoryCountStr <- getLine
    let factoryCount = read factoryCountStr :: Int
    linkCountStr <- getLine
    let linkCount = read linkCountStr :: Int
    graph <- initGraph linkCount empty
    gameLoop graph