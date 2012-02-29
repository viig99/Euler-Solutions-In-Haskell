module PrimeHelper where
import Data.Numbers.Primes 
import Data.Digits

percentagePrimes :: (Ord a,Integral a) => a -> Double
percentagePrimes i = (fromIntegral $ length $ takeWhile (<=i) primes) / (fromIntegral i) * 100

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
	| even n = n : collatz (div n 2)
	|  odd n = n : collatz (3*n + 1)

factorial :: (Integral a) => a -> a  
factorial n = product [1..n]

fibonnaci :: Int -> Integer
fibonnaci = (map f [0 ..] !!)
   where f 0 = 0
         f 1 = 1
         f n = fibonnaci (n-2) + fibonnaci (n-1)

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

digitsInfib :: Int -> Int
digitsInfib = length . digits 10 . fibonnaci

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n l = take n l : groupsOf n (tail l)