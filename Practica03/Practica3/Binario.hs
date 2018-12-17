module Binario where 

 --Ejercicio 1.1
 data Binario = BaseUno | Cero Binario | Uno Binario deriving (Eq)

 instance Show Binario where
  show BaseUno = "1"
  show (Cero b) = show b ++ "0"
  show (Uno b) = show b ++ "1"

--Ejercicio 1.2
 natToBin :: Int -> Binario
 --Tu código va aquí
 natToBin 1 = BaseUno
 natToBin n 
  |mod n 2 == 0 = Cero (natToBin (div n 2)) 
  |mod n 2 == 1 = Uno (natToBin (div n 2)) 


--Ejercicio 1.3
 binToNat :: Binario -> Int
 --Tu código va aquí
 binToNat BaseUno = 1
 binToNat (Cero bin) = 2 * (binToNat bin)
 binToNat (Uno bin) =  1 + 2 * (binToNat bin)


--Ejercicio 1.4
 sucesor :: Binario -> Binario
 --Tu código va aquí
 sucesor BaseUno = Cero BaseUno
 sucesor (Cero bin) = Uno bin 
 sucesor (Uno bin) = Cero (sucesor bin)

--Ejercicio 1.5
 bitsEncendidos :: Binario -> Int
 --Tu código va aquí
 bitsEncendidos BaseUno = 1
 bitsEncendidos (Cero bin) = bitsEncendidos bin
 bitsEncendidos (Uno bin) = 1 + bitsEncendidos bin