--ESTRUCTURAS DISCRETAS 2019-1
--Práctica 2

module Practica2 where

--DEFINICIÓN DE FUNCIONES

--Ejercicio 2.1:
fibonacci :: Int -> Int
--Tu código va aquí
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)