import PrimeHelper
import Data.List
import qualified Data.Set as Set

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let s = quicksort (filter (<=x) xs)
        b = quicksort (filter (>x) xs)
    in  s ++ [x] ++ b

largestnum :: (Integral a) => a -> a
largestnum y = head (filter p [100000,99999..])
		where p x = mod x y == 0

sum' :: (Integral a) => [a] -> a
sum' = foldl (+) 0

main = putStrLn . show . fibonnaci $ 50000
