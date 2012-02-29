-- This problem is under construction, imperative programming conflict with functional one.
import Data.Array


initArray = listArray ((1,1),(20,20)) . map (\c -> read c :: Integer) . words
 
main = do
		 t <- readFile "depen_files/euler11"
		 putStrLn . show . initArray $ t