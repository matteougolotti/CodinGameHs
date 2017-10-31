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

bestMove :: Map Int (Map Int Int) -> Map Int Factory -> Map Int Troop -> [Move] -> Move
bestMove _ _ _ [] = Move WAIT 0 0 0
bestMove graph factories troops moves =
    List.minimumBy (\ m1 m2 -> if factoryCyborgs (factories ! moveDst m1) > factoryCyborgs (factories ! moveDst m2)
                               then GT
                               else LT) moves

neighbors :: Map Int (Map Int Int) -> Int -> [Int]
neighbors graph src = keys (graph ! src)

generateMovesFromSrc :: Map Int Factory -> Map Int Troop -> Int -> [Int] -> [Move]
generateMovesFromSrc factories troops src = 
    Prelude.foldl (\ moves dst -> (Move MOVE src dst 1 : moves)) []

generateMoves :: Map Int (Map Int Int) -> Map Int Factory -> Map Int Troop -> [Move]
generateMoves graph factories troops = do
    let sources = Map.filter (\ f -> factoryOwner f == 1 && factoryCyborgs f > 1) factories
    let validSrcIds = Prelude.map factoryId (elems sources)
    Prelude.foldl (\ moves src -> do
        let destinations = neighbors graph src
        let validDstIds = List.filter (\ dst -> factoryOwner (factories!dst) /= 1) destinations
        generateMovesFromSrc factories troops src validDstIds ++ moves) [] validSrcIds

graphDoubleLink :: Map Int (Map Int Int) -> Int -> Int -> Int -> Map Int (Map Int Int)
graphDoubleLink graph s d v = do
    let g = if member s graph
            then Map.insert s (Map.insert d v (graph ! s)) graph
            else Map.insert s (Map.insert d v empty) graph
    if member d g
    then Map.insert d (Map.insert s v (g ! d)) g
    else Map.insert d (Map.insert s v empty) g

initGraph :: Int -> Map Int (Map Int Int) -> IO (Map Int (Map Int Int))
initGraph 0 m = return m
initGraph n m = do
    linkStr <- getLine
    let link = linkStr |> words |> Prelude.map read :: [Int]
    if member (head link) m
    then initGraph (n - 1) (graphDoubleLink m (head link) (link!!1) (link!!2))
    else initGraph (n - 1) (graphDoubleLink empty (head link) (link!!1) (link!!2))

readEntities :: Map Int Factory -> Map Int Troop -> Int -> IO (Map Int Factory, Map Int Troop)
readEntities factories troops 0 = return (factories, troops)
readEntities factories troops count = do
    entityStr <- getLine
    let inputs = entityStr |> words
    let entityId = read (head inputs) :: Int
    let entityType = inputs!!1
    let arg1 = read (inputs!!2) :: Int
    let arg2 = read (inputs!!3) :: Int
    let arg3 = read (inputs!!4) :: Int
    let arg4 = read (inputs!!5) :: Int
    let arg5 = read (inputs!!6) :: Int
    case entityType of
        "FACTORY" -> readEntities (Map.insert entityId (Factory entityId arg1 arg2 arg3) factories) troops (count - 1)
        "TROOP" -> readEntities factories (Map.insert entityId (Troop entityId arg1 arg2 arg3 arg4 arg5) troops) (count - 1)

gameLoop :: Map Int (Map Int Int) -> IO ()
gameLoop graph = do
    entityCountStr <- getLine
    let entityCount = read entityCountStr :: Int
    (factories, troops) <- readEntities empty empty entityCount
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
