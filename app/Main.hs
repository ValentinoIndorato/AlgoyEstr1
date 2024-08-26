
-- ls app muestra las cosas dentro de app
-- cd app entre a app
-- cat Main.hs muestra dentro de main
-- gchi para entrar
-- ctrl d salir
-- :l app/Main.hs para entrar
-- :r recargar
--3 b)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
--3 c)
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs
--3 d)
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)
--3 e)
promedio :: [Int] -> Int
promedio []=0
promedio (x:xs) =  sumatoria (x:xs) `div` length(x:xs)
-- Laboratorio 3
-- 4a)
f4a :: [Int] -> Bool
f4a [] = True
f4a (x:xs) = x > 0 && f4a xs
-- 4b)
f4b :: ([Int],Int) -> Bool
f4b ([],x) = True
f4b ((y:ys),x) = y==x || f4b (ys,x)
-- 4d)
f4d :: [Int]-> Bool
f4d [] = True

-- Laboratorio  4
--b)
--- 7a)
f7a :: Int -> Int 
f7a x | x==0 = 1 
 |  x>0 = x * f7a (x-1)
--- 7b)
f7b :: [Int] -> Int
f7b [] = 0
f7b xs = (sumatoria xs) `div` length xs
--- 7c)
f7c :: ([Int], [Int]) -> Bool
f7c ([],[]) = False
f7c (xs,[]) = False
f7c ([],ys) = True
f7c (xs, ys) = f7cMax xs < f7cMin ys

f7cMax :: [Int]->Int
f7cMax [] = -100 
f7cMax (x:xs) = x `max` f7cMax xs

f7cMin :: [Int]->Int
f7cMin [] = 1000
f7cMin (x:xs) = x `min` f7cMin xs

--- 7d)
f7d :: (Int,Int,Int) -> Bool
f7d (a,b,c)= (2 <= a && a< c) &&( 2 <= a&& a < c )&& a*b == c

