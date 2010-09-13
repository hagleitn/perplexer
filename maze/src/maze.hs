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
  show (Game (Maze m, h, (p:ps), l)) = (getOptions $ rotateWalls h (m!p))

printHorizontalWall True (_,True,_,_)   = do { putStr " -" }
printHorizontalWall True (_,False,_,_)  = do { putStr "  " }
printHorizontalWall False (_,_,_,True)  = do { putStr " -" }
printHorizontalWall False (_,_,_,False) = do { putStr "  " }

printVerticalWall m s e p ix = do
  (a,_,_,_) <- return (m!ix)
  start     <- return (s == ix)
  exit      <- return (e == ix)
  path      <- return (ix `elem` p)
  x         <- return (case (start,exit,path) of
                 (True,_,_) -> 's'
                 (_,True,_) -> 'e'
                 (_,_,True) -> '*'
                 _ -> ' ')
  case a of
    True  -> putStr ['|',x]
    False -> putStr [' ',x]
    
printHorizontal i u (Maze a) = do 
  ((_,_),(n,m)) <- return $ bounds a
  forM_ [ (a!(i,j)) | j <- [1..m] ] (printHorizontalWall u)
  putStrLn " "

printVertical i s e p (Maze a) = do
  ((_,_),(n,m)) <- return $ bounds a
  forM_ [ (i,j) | j <- [1..m] ] (printVerticalWall a s e p)
  putStrLn "|"

printSection m s e p i = do
  printVertical i s e p m
  printHorizontal i False m
      
printMaze m@(Maze a) s e p = do
  ((_,_),(n,_)) <- return $ bounds a
  printHorizontal n True m
  forM_ [n,(n-1)..1] (printSection m s e p)
  hFlush stdout
  
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
  i <- return $ increaseLocation m h p1
  put (Game (Maze m, h, i:p, l))

next (TURN d) = do
  (Game (m, h, p, l)) <- get
  put (Game (m, (rotateHeading h d), p, l))

next LOOK = do return ()

next (NEW n m) = do put (mkGame n m)

next SHOW = do
 (Game (m, h, p, l)) <- get
 lift $ printMaze m (last p) l p

loop = forever (do
  l <- lift getLine
  c <- return $ reads l
  case c of
    [] -> return ()
    (c1,_):cs -> next c1
  g@(Game (m@(Maze a),h,(p:ps),l)) <- get
  if (p == l)
    then
      do
        ((_,_),(n,m)) <- return $ bounds a
        lift $ print "Congratulations. You did it."
        ng <- return $ mkGame n m
        put (ng)
        lift $ print ng
    else
     lift $ print g)
  
main = do
  (n:m:[]) <- getArgs
  g <- return $ mkGame (read m) (read n)
  print g
  s <- execStateT loop g
  print s
