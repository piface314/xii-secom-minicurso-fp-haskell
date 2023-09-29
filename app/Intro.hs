module Intro where

import           Data.List (partition)

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib2 :: Int -> Int
fib2 0 = 0
fib2 1 = 1
fib2 n = fib' 0 1 2
    where fib' n2 n1 i =
            if i == n
                then n2 + n1
                else fib' n1 (n2+n1) (i+1)

fib3 :: Int -> Int
fib3 = (fibSeq !!)
    where fibSeq = 0 : 1 : zipWith (+) fibSeq (tail fibSeq)

bhaskara :: Double -> Double -> Double -> [Double]
bhaskara a b c
  | delta < 0  = []
  | delta == 0 = [x']
  | otherwise  = [x', x'']
  where
      delta = b**2 - 4*a*c
      x' = (-b + sqrt delta) / (2*a)
      x'' = (-b - sqrt delta) / (2*a)


-- duas opções de implementar o quicksort
quicksort :: Ord a => [a] -> [a]
quicksort []         = []
quicksort (pivot:xs) = quicksort lt ++ [pivot] ++ quicksort gt
    where (lt,gt) = partition (< pivot) xs

quicksort2 :: Ord a => [a] -> [a]
quicksort2 []         = []
quicksort2 (pivot:xs) = quicksort2 lt ++ [pivot] ++ quicksort2 gt
    where (lt,gt) = foldr (\x (lt,gt) -> if x < pivot then (x:lt,gt) else (lt,x:gt)) ([], []) xs
