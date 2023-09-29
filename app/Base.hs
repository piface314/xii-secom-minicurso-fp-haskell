{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
module Base where

import           Data.Char (isUpper, toUpper)

---------------------------------

-- Obtém as iniciais de cada string
initials :: [String] -> [Char]
initials []     = []
initials (s:s') = toUpper (head s) : initials s'

-- Obtém os dígitos das unidades de cada inteiro
digits :: [Int] -> [Int]
digits []     = []
digits (x:xs) = x `mod` 10 : digits xs

-- Implementação da função map
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

---------------------------------

-- Filtrar string para manter apenas caracteres em caixa alta
onlyUpper :: String -> String
onlyUpper "" = ""
onlyUpper (c:s)
    | isUpper c = c : onlyUpper s
    | otherwise = onlyUpper s

-- Filtrar inteiros para manter apenas divisíveis por 10
onlyDivBy10 :: [Int] -> [Int]
onlyDivBy10 [] = []
onlyDivBy10 (x:xs)
    | x `mod` 10 == 0 = x : onlyDivBy10 xs
    | otherwise       = onlyDivBy10 xs

-- Implementação da função filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' f (x:xs)
    | f x       = x : filter' f xs
    | otherwise = filter' f xs

---------------------------------


-- Duas formas de implementar a função de tamanho da lista...
-- .. indo da direita para a esquerda
lengthR :: [a] -> Int
lengthR []     = 0
lengthR (_:xs) = 1 + lengthR xs

-- .. ou da esquerda para a direita
lengthL :: [a] -> Int
lengthL = length' 0
    where length' n []     = n
          length' n (_:xs) = length' (n+1) xs

-- as duas opções da função fold
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z []     = z
foldr' f z (x:xs) = f x (foldr' f z xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z []     = z
foldl' f z (x:xs) = foldl' f (f z x) xs

---------------------------------

-- para exemplificar o uso do Maybe
lookupBy :: (a -> Bool) -> [a] -> Maybe a
lookupBy f [] = Nothing
lookupBy f (h:t) =
    if f h
        then Just h
        else lookupBy f t