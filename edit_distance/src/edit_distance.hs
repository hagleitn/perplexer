import Data.List
import Data.Array

instance (Ord a) => Ix [a] where
  range (m,n) = map (\i -> [ undefined | _ <- [1..i]]) [(length m)..(length n)]
  index (m,n) i = index (length m, length n) (length i)
  inRange (m,n) i = inRange (length m, length n) (length i)

editDistance delCost insCost editCost v w =
  let
    mkIns (x,(y:ys)) = (c+(insCost y), (y:s):r)
      where (c,r@(s:rs)) = memoized_dist (x,ys)

    mkDel ((x:xs),y) = (c+(delCost x), ss:r)
      where
        (c,res) = memoized_dist (xs,y)
        r@((s:ss):rs) = map ((:) x) res

    mkEdit ((x:xs),(y:ys))
        | x == y = (c,r)
        | otherwise = (c+(editCost x y),(y:ss):r)
      where
        (c,res) = memoized_dist (xs,ys)
        r@((s:ss):rs) = map ((:) x) res
    
    memoized_dist =
      let        
        dist ([],[]) = (0,[[]])
        dist k@([],y) = mkIns k
        dist k@(x,[]) = mkDel k
        dist k@(x,y)  = minimum [ mkIns k, mkDel k, mkEdit k ]
      in
        (fmap dist (array (([],[]),(v,w)) [ ((i,j),(i,j)) | i <- (tails v), j <- (tails w) ]) !)

    rev (a,b) = (a, reverse b)
  in
    (rev . memoized_dist) (v,w)

editDist v w = editDistance (const 2) (const 3) (\i j -> 1) v w
