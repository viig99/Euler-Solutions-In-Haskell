111import PrimeHelper
import Data.Numbers.Primes
import Data.Digits
import Data.Char

euler1 = foldl1 (+) . takeWhile (<1000) $ [x | x <- [1..], mod x 3 == 0 || mod x 5 == 0]

euler2 = foldl1 (+) . takeWhile (<4*10^6) . filter even $ fibs

euler3 = maximum . primeFactors $ 600851475143

euler4 = maximum [x | y <- [100..999] , z <- [y..999], let x = y * z , let s = show x, s == reverse s]

euler5 = foldl1 (*) [1..20]

euler6 = abs $ (sum . take 100 . map (^2) $ [1..]) - (sum . take 100 $ [1..])^2

euler7 = primes !! 10000

euler8 = do
			t <- readFile "depen_files/euler8"
			putStrLn . show . maximum . map product . groupsOf 5 . map digitToInt . concat . lines $ t

euler9 = product . head $ [[a,b,c]|a<-[1..1000],b<-[1..1000],let c=(1000-a-b),c > 0,c^2==(a^2+b^2)]

euler10 = sum . takeWhile (<2*10^6) $ primes
