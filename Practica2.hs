--ESTRUCTURAS DISCRETAS 2019-1
--Práctica 2

module Practica2 where

--DEFINICIÓN DE LISTAS

--1.1: Naturales.
nat = [0..]
--1.2: Multiplos de diez.
multiplosDiez = [0,10..]
--1.3: potencias de 2.
potenciasDos = [2 ** x | x <- nat]
--1.4: Números pares.
pares = [2 * x | x <- nat]
--1.5: años desde el año de tu nacimiento.
anosVividos = [1995..2018]


--DEFINICIÓN DE FUNCIONES

--Ejercicio 2.1:
fibonacci :: Int -> Int
--Tu código va aquí
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--Ejercicio 2.2:
elemento :: (Eq a) => [a] -> a -> Bool
--Tu código va aquí
elemento [] x = False
elemento (x:xs) a = x==a || elemento (xs) a

--Ejercicio 2.3:
sumaLista ::(Num a) => [a] -> a
--Tu código va aquí
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista(xs)

--Ejercicio 2.4:
meses :: [Int] -> [String]
--Tu código va aquí
mesesLista = ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
              "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"]
meses [] = []
meses (x:xs) = [mesesLista!!x] ++ meses(xs)

--Ejercicio 2.5:
divisoresPropios :: Int -> [Int]
--Tu código va aquí
divisoresPropios n = [ x | x <- [1..n-1], mod n x == 0 ]

--Ejercicio 2.6:
esPerfecto :: Int -> Bool
--Tu código va aquí
esPerfecto n = n == sumaLista (divisoresPropios n)

--Ejercicio 2.7:
sonAmigos :: Int -> Int -> Bool
--Tu código va aquí
sonAmigos a b= a == sumaLista (divisoresPropios b) &&
               b == sumaLista (divisoresPropios a)

--Ejercicio 2.8:
supersuma :: Int -> Int
--Tu código va aquí
supersuma x = sumaLista (intToList x)

intToList :: Integral x => x -> [x]
intToList 0 =[]
intToList x = intToList (div x 10) ++ [mod x 10]

--Ejercicio 2.9:
japones :: Int -> String
--Tu código va aquí
numeros = ["rei", "ichi", "ni", "san", "yon", "go", "roku", "nana", "haci",
           "kyu", "ju"]
japones x = if x<=10 then numeros!!x else
            numeros!!((intToList x)!!0) ++ " ju " ++
            if (intToList x)!!1>0 then numeros!!((intToList x)!!1) else ""
