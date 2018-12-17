--ESTRUCTURAS DISCRETAS 2019-1
--Práctica 1

--Ejercicio 1.1:
areaCirculo :: Float -> Float
--Tu código va aquí
areaCirculo radio = pi * radio ^ 2

--Ejercicio 1.2:
distancia :: (Float , Float ) -> (Float , Float ) -> Float
--Tu código va aquí
distancia (x1 , y1) (x2 , y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

--Ejercicio 1.3:
imp :: Bool -> Bool -> Bool
--Tu código va aquí
imp bol1 bol2 = not bol1 || bol2

--Ejercicio 1.4:
xor :: Bool -> Bool -> Bool
--Tu código va aquí
xor a b = (a || b) && not (a && b)

--Ejercicio 1.5:
mes :: Int -> String
--Tu código va aquí
mes 1 = "Enero"
mes 2 = "Febrero"
mes 3 = "Marzo"
mes 4 = "Abril"
mes 5 = "Mayo"
mes 6 = "Junio"
mes 7 = "Julio"
mes 8 = "Agosto"
mes 9 = "Septiembre"
mes 10 = "Octubre"
mes 11 = "Noviembre"
mes 12 = "Diciembre"

--Ejercicio 1.6:
calculadora :: String -> (Int ,Int) -> Int
--Tu código va aquí
calculadora "first" (a,b) = min a b
calculadora "last" (a,b) = b
calculadora "sum" (a,b) = a + b
calculadora "rest" (a,b) = a - b
calculadora "mul" (a,b) = a * b
calculadora "div" (a,b) = div a b
calculadora "pow" (a,b) = a ^ b

--Ejercicio 1.7:
loki :: Int -> Bool -> String
--Tu código va aquí
loki a b = if (20 <= a && a <= 30 && b) ||  (15 <= a && a <= 25 && not b) then "Sale a jugar" else "No sale a jugar"

--Ejercicio 1.8:
monos :: Bool -> Bool -> String
--Tu código va aquí
monos a b = if imp a b && imp b a then "Hay problemas" else "No hay problemas"

--Ejercicio 1.9:
multiplica :: Int -> Int -> Int
--Tu código va aquí
multiplica _ 0 = 0
multiplica n m = (multiplica n (m - 1)) + n

--Ejercicio 1.10:
potencia :: Int -> Int -> Int
--Tu código va aquí
potencia _ 0 = 1
potencia n m = multiplica a n
    where
        a = potencia n (m - 1)
