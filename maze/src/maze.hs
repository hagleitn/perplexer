import System
import System.IO
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Char
import Data.Array
import Data.List
import System.Random ( randomR, mkStdGen, StdGen )

data Direction = LEFT | RIGHT deriving (Show, Eq, Ord, Read)
data Heading = N | W | S | E deriving (Show, Eq, Ord)
data Command = MOVE | TURN Direction | LOOK | NEW Int Int | SHOW | RESET deriving (Show, Eq, Ord, Read)
type Walls = (Bool, Bool, Bool, Bool)
type Location = (Int,Int) 
type Path = [Location]
newtype Maze = Maze (Array Location Walls) deriving (Show, Eq, Ord)
newtype Game = Game (Maze, Heading, Path, Location, StdGen)

chooseLocation cs g =
  let
    n = length cs
    (i,ng) = randomR (0,n-1) g
  in 
    (cs!!i,ng)

mergeWalls (loc,(l,a,r,b)) (loc1,(l1,a1,r1,b1)) = (loc,(l&&l1,a&&a1,r&&r1,b&&b1))

mergeUpdates us =
  let
    firstEq (l,_) (r,_) = l == r
    firstOrd (l,_) (r,_) = compare l r
    ss = sortBy firstOrd us
    gs = groupBy firstEq ss
  in
    map (foldr mergeWalls ((1,1),(True, True, True, True))) gs

removeWall (l,a,r,b) (l1,a1,r1,b1) loc1@(i,j) loc2@(n,m)
  | j == m+1 = [(loc1, (False,a,r,b)), (loc2, (l1,a1,False,b1))]
  | j == m-1 = [(loc1, (l,a,False,b)), (loc2, (False,a1,r1,b1))]
  | i == n+1 = [(loc1, (l,a,r,False)), (loc2, (l1,False,r1,b1))]
  | i == n-1 = [(loc1, (l,False,r,b)), (loc2, (l1,a1,r1,False))]

getNeighbors (i,j) mazeElements n m =
  let
    ls = [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
    fs = filter (inRange ((1,1),(n,m))) ls
    xs = filter (\i -> not $ elem i mazeElements) fs
  in
    map (\nl -> ((i,j),nl)) xs
  
mkPaths _ [] _ _ _ g = ([],g)
mkPaths maze@(Maze arr) candidates mazeElements n m g =
  let
    ((l,k),g2) = chooseLocation candidates g
    ws = removeWall (arr!l) (arr!k) l k
    ln = getNeighbors k mazeElements n m
    nme = (k:mazeElements)
    nc = filter (\(j,i) -> not $ elem i nme) candidates
    (ps,g3) = mkPaths maze (ln++nc) nme n m g2
  in
    (ws++ps,g3)
  
mkMaze n m s g =
  let
    arr = array ((1,1),(n,m)) [ ((i,j),(True,True,True,True)) | i <- [1..n], j <- [1..m] ]
    maze = Maze arr
    nbs = getNeighbors s [s] n m
    (ps,g2) = mkPaths maze nbs [s] n m g
  in
    (Maze $ arr // (mergeUpdates ps),g2)

mkGame n m g =
  let
    (x1,g1) = randomR (1,n) g
    (x2,g2) = randomR (1,m) g1
    (x3,g3) = randomR (1,n) g2
    (x4,g4) = randomR (1,m) g3
    start = (x1,x2)
    end = (x3,x4)
    (maze,g5) = mkMaze n m start g4
  in
    Game (maze, N, [start], end, g5)

wallOrWay b | True = "wall" | False = "corridor"

getOptions (l,a,r,_) = "There is a "++(wallOrWay l)++" to the left, a "++(wallOrWay a)++" ahead, and a "++(wallOrWay r)++" to the right."

instance Show Game where
  show (Game (Maze m, h, (p:_), _, _)) = getOptions $ rotateWalls h $ m!p

printHorizontalWall True  (_,True,_,_)   = " -" 
printHorizontalWall True  (_,False,_,_)  = "  "
printHorizontalWall False (_,_,_,True)   = " -"
printHorizontalWall False (_,_,_,False)  = "  "

printVerticalWall ix = do
  Game (Maze m, h, p, e, _) <- get
  let
    (a,_,_,_) = m!ix
    start     = (last p) == ix
    exit      = e == ix
    path      = ix `elem` p
    x = case  (start,exit,path) of
              (True,_,_) -> 's'
              (_,True,_) -> 'e'
              (_,_,True) -> '*'
              _          -> ' '
  case a of
    True  -> lift $ putStr ['|',x]
    False -> lift $ putStr [' ',x]
    
printHorizontal m i u = do
  Game (Maze a, _, _, _, _) <- get
  forM_ [ (a!(i,j)) | j <- [1..m] ] $ lift . putStr . (printHorizontalWall u)
  lift $ putStrLn " "

printVertical m i = do
  forM_ [ (i,j) | j <- [1..m] ] printVerticalWall
  lift $ putStrLn "|"

printSection m i = do
  printVertical m i
  printHorizontal m i False
      
printMaze = do
  (Game (Maze a, _, _, _, _)) <- get
  let ((_,_),(n,m)) = bounds a
  printHorizontal m n True
  forM_ [n,(n-1)..1] $ printSection m
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
  Game (Maze m, h, p@(p1:ps), l, g) <- get
  let i = increaseLocation m h p1
  put $ Game (Maze m, h, i:p, l, g)

next (TURN d) = do
  Game (m, h, p, l, g) <- get
  put $ Game (m, rotateHeading h d, p, l, g)

next LOOK = return ()

next (NEW n m) = do
  Game (_,_,_,_,g) <- get
  put $ mkGame n m g

next SHOW = printMaze

next RESET = do
  Game (m, _, p, l, g) <- get
  put $ Game (m, N, [(last p)], l, g)

checkSuccess = do
  Game (Maze a, _, p:ps, l, gen)<- get
  if p == l
    then
      do
        lift $ print "Congratulations. You did it."
        lift $ print "Starting new game..."
        let
          ((_,_),(n,m)) = bounds a        
          ng = mkGame n m gen
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
  (n:m:seed:[]) <- getArgs
  s <- execStateT loop $ mkGame (read m) (read n) $ mkStdGen (read seed)
  return ()