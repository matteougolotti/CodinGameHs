import System.IO
import Data.Map

(|>) x f = f x

data Factory = Factory {
    factoryId :: Int,
    factoryOwner :: Int,
    factoryCyborgs :: Int,
    production :: Int
} deriving (Show)

data Troop = Troop {
    troopId :: Int,
    troopOwner :: Int,
    src :: Int,
    dst :: Int,
    troopCyborgs :: Int,
    turnsLeft :: Int
} deriving (Show)

data MoveType = MOVE | WAIT
  deriving (Show)

data Move = Move {
    move :: MoveType,
    source :: Int,
    destination :: Int,
    moveCyborgs :: Int
} deriving (Show)

initGraph :: Int -> Map Int (Map Int Int) -> IO (Map Int (Map Int Int))
initGraph 0 m = return m
initGraph n m = do
    linkStr <- getLine
    let link = linkStr |> words |> Prelude.map read :: [Int]
    if member (head link) m
    then initGraph (n - 1) (insert (head link) (insert (link !! 1) (link !! 2) (m ! head link)) m)
    else initGraph (n - 1) (insert (head link) (insert (link !! 1) (link !! 2) empty) m)

main :: IO ()
main= do
    factoryCountStr <- getLine
    let factoryCount = read factoryCountStr :: Int
    linkCountStr <- getLine
    let linkCount = read linkCountStr :: Int
    ()