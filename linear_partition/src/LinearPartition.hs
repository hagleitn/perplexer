import Data.List
import Data.Array
import Data.Maybe

data (Num a, Ord a) => Partition a = Partition { k :: Int, max_sum :: a, sums :: [a], parts :: [[a]] }
 deriving (Eq,Show)

instance (Num a, Ord a) => Ord (Partition a) where
 (Partition _ s _ _) <= (Partition _ t _ _) = s >= t

part :: (Num a, Ord a) => [a] -> Int -> Partition a
part l k =
 let
   n = length l
   merge [] _ = Nothing
   merge xs (Partition k s ss ps) = return (Partition (k+1) (max (sum xs) s) (ss++[sum xs]) (ps++[xs]))
   memoized_part =
     let
       part (1,m) = return (Partition 1 (sum nl) [sum nl] [nl]) where nl = (take m l)
       part (k,m) = maximum [ memoized_part (k-1,j) >>= (merge . (drop j) . (take m)) l | j <- [1..m] ]
     in
       (fmap part (array ((1,1),(k,n)) [ ((i,j),(i,j)) | i <- [1..k], j <- [1..n]]) !)
 in
   (fromJust . memoized_part) (k,n)
