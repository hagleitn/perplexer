import Data.List
import Data.Array
import System

instance (Ord a) => Ix [a] where
  range (m,n) = map (\i -> [ undefined | _ <- [1..i]]) [(length m)..(length n)]
  index (m,n) i = index (length m, length n) $ length i
  inRange (m,n) i = inRange (length m, length n) $ length i

editDistance :: (Num b, Ord b, Ord a) => (a -> b) -> (a -> b) -> (a -> a -> b) -> [a] -> [a] -> (b,[[a]])
editDistance delCost insCost editCost v w =
  let
    ins (x,(y:ys)) = (insCost y + c, (y:s):r)
      where (c,r@(s:rs)) = memoized_dist (x,ys)

    del ((x:xs),y) = (delCost x + c, ss:r)
      where
        (c,res) = memoized_dist (xs,y)
        r@((s:ss):rs) = map ((:) x) res

    edit ((x:xs),(y:ys))
        | x == y = (c,r)
        | otherwise = (editCost x y + c, (y:ss):r)
      where
        (c,res) = memoized_dist (xs, ys)
        r@((s:ss):rs) = map ((:) x) res
    
    memoized_dist =
      let        
        dist ([],[]) = (0,[[]])
        dist k@([],y) = ins k
        dist k@(x,[]) = del k
        dist k  = minimum [ ins k, del k, edit k ]
      in
        (fmap dist (array (([],[]),(v,w)) [ ((i,j),(i,j)) | i <- tails v, j <- tails w ]) !)

    rev (a,b) = (a,reverse b)
  in
    (rev . memoized_dist) (v,w)

editDist :: (Num b, Ord b, Ord a) => [a] -> [a] -> (b,[[a]])    
editDist v w = editDistance (const 2) (const 3) (\i j -> 1) v w

main :: IO ()
main = do
  (a:b:[]) <- getArgs
  print $ editDist a b
