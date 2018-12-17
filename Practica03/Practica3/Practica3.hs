module Practica3 where 

 import Binario
 import Practica2

--Ejercicio 2.1
 binarios :: [Int] -> [Binario]
 --Tu código va aquí
 binarios b = map (natToBin) b

--Ejercicio 2.2
 pares :: [Binario] -> [Binario]
 --Tu código va aquí
 pares b = filter (\x -> mod (binToNat x) 2 == 0 ) b

--Ejercicio 2.3
 tooLong :: [String] -> [String]
 --Tu código va aquí
 tooLong s = filter ( \x -> (length x) <= 7 ) s 

--Ejercicio 2.4
 sFibonacci :: Int -> [Int]
 sFibonacci x = map (fibonacci) [0..x]

--Ejercicio 2.5
 quitaElemento :: (Eq a) => [a] -> a -> [a]
 --Tu código va aquí
 quitaElemento l e = filter ( \x -> x /= e ) l 

--Extra
 --Ejercicio 1.1
 data Binario2 = BaseCero | Cero2 Binario2 | Uno2 Binario2 deriving (Eq)

 instance Show Binario2 where
  show BaseCero = "0"
  show (Cero2 b) = show b ++ "0"
  show (Uno2 b) = show b ++ "1"