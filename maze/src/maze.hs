import System
import System.IO
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Char
import Data.Array

data Direction = LEFT | RIGHT deriving (Show, Eq, Ord, Read)
data Heading = N | W | S | E deriving (Show, Eq, Ord)
data Command = MOVE | TURN Direction | LOOK | NEW Int Int | SHOW deriving (Show, Eq, Ord, Read)
type Walls = (Bool, Bool, Bool, Bool)
type Location = (Int,Int) 
type Path = [Location]
newtype Maze = Maze (Array Location Walls) deriving (Show, Eq, Ord)
newtype Game = Game (Maze, Heading, Path, Location)

mkWall 1 1 _ _ = (True,False,False,True)
mkWall 1 j _ m
  | j == m = (False,False,True,True)
  | otherwise = (False,False,False,True)
mkWall i 1 n m
  | i == n = (True,True,False,False)
  | otherwise = (True,False,False,False)
mkWall i j n m
  | i == n && j == m = (False,True,True,False)
  | i == 1 && j == m = (False,False,True,True)
  | i == n && j == 1 = (True,True,False,False)
  | i == n = (False,True,False,False)
  | j == m = (False,False,True,False)
  | otherwise = (False,False,False,False)

mkMaze :: Int -> Int -> Maze
mkMaze n m = Maze (array ((1,1),(n,m)) [ ((i,j),(mkWall i j n m)) | i <- [1..n], j <- [1..m] ])

mkGame n m = Game (mkMaze n m, N, [(1,1)], (n,m))

wallOrWay b = if b then "Wall" else "Corridor"

getOptions (l,a,r,_) = "There is a "++(wallOrWay l)++" to the left, a "++(wallOrWay a)++" ahead, and a "++(wallOrWay r)++" to the right."

instance Show Game where
  show (Game (Maze m, h, (p:ps), l)) = getOptions $ rotateWalls h (m!p)

printHorizontalWall True (_,True,_,_)   = " -" 
printHorizontalWall True (_,False,_,_)  = "  "
printHorizontalWall False (_,_,_,True)  = " -"
printHorizontalWall False (_,_,_,False) = "  "

printVerticalWall ix = do
  (Game (Maze m, h, p, e)) <- get
  let
    (a,_,_,_) = m!ix
    start     = (last p) == ix
    exit      = e == ix
    path      = ix `elem` p
    x = case (start,exit,path) of
          (True,_,_) -> 's'
          (_,True,_) -> 'e'
          (_,_,True) -> '*'
          _          -> ' '
  case a of
    True  -> lift $ putStr ['|',x]
    False -> lift $ putStr [' ',x]
    
printHorizontal m i u = do
  (Game (Maze a, _, _, _)) <- get
  forM_ [ (a!(i,j)) | j <- [1..m] ] (lift . putStr . (printHorizontalWall u))
  lift $ putStrLn " "

printVertical m i = do
  forM_ [ (i,j) | j <- [1..m] ] (printVerticalWall)
  lift $ putStrLn "|"

printSection m i = do
  printVertical m i
  printHorizontal m i False
      
printMaze = do
  (Game (Maze a, _, _, _)) <- get
  let ((_,_),(n,m)) = bounds a
  printHorizontal m n True
  forM_ [n,(n-1)..1] (printSection m)
  lift $ hFlush stdout
  
rotateWalls N w = w
rotateWalls W (l,a,r,b) = (b,l,a,r)
rotateWalls S (l,a,r,b) = (r,b,l,a)
rotateWalls E (l,a,r,b) = (a,r,b,l)

rotateHeading h LEFT = case h of { N -> W; W -> S; S -> E; E -> N }

rotateHeading h RIGHT = case h of { N -> E; W -> N; S -> W; E -> S }

increaseLocation m h p@(i,j) =
    let (l,a,r,b) = m!p in
      case h of
        N -> if a then p else (i+1,j)
        W -> if l then p else (i,j-1)
        S -> if b then p else (i-1,j)
        E -> if r then p else (i,j+1)
    
next MOVE = do
  (Game (Maze m, h, p@(p1:ps), l)) <- get
  let i = increaseLocation m h p1
  put (Game (Maze m, h, i:p, l))

next (TURN d) = do
  (Game (m, h, p, l)) <- get
  put (Game (m, (rotateHeading h d), p, l))

next LOOK = return ()

next (NEW n m) = put (mkGame n m)

next SHOW = printMaze

checkSuccess = do
  g@(Game (m@(Maze a),h,(p:ps),l))<- get
  if (p == l)
    then
      do
        lift $ print "Congratulations. You did it."
        lift $ print "Starting new game..."
        let
          ((_,_),(n,m)) = bounds a        
          ng = mkGame n m
        put ng
    else
      return ()

loop = forever (do
  checkSuccess

  -- print state
  g <- get
  lift $ print g
  lift $ hFlush stdout

  -- get next command and execute
  l <- lift getLine
  c <- return $ reads l
  case c of
    [] -> return ()
    (c1,_):cs -> next c1)
  
main = do
  (n:m:[]) <- getArgs
  s <- execStateT loop $ mkGame (read m) (read n)
  return ()