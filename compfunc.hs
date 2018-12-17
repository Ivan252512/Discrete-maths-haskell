import Data.List (nub)
 
composicion :: Eq a => [(a,a)] -> [(a,a)] -> [(a,a)]
composicion r s = nub [(x,y) | (x,u) <- r, (v,y) <- s, u == v]