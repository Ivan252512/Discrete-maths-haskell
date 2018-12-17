module Snoc where

 --Tipo de dato algebraico para definir listas Snoc
 data SnocList a = Mt
                 | Snoc (SnocList a) a
                 deriving (Eq, Ord, Show)

 --Ejercicio1.1
 -- Agrega un elemento
 addSnoc :: SnocList a -> a -> SnocList a
 addSnoc Mt a = (Snoc Mt a)
 addSnoc s a = (Snoc s a)

 --Ejercicio1.2
 --Regresa el último
 ultimo :: SnocList a -> a
 ultimo (Snoc x a) = a  

 --Ejercicio1.3
 --Regresa el resto.
 resto :: SnocList a -> SnocList a
 resto (Snoc x a) = x 

 --Ejercicio1.4
 --Regresa la cabeza de la lista Snoc. 
 cabeza :: SnocList a -> a
 cabeza (Snoc Mt a) = a
 cabeza (Snoc x a) = cabeza x

 --Ejercicio1.5
 --Regresa la colade lalista Snoc.
 cola :: SnocList a -> SnocList a
 cola (Snoc (Snoc Mt a) b) = Snoc Mt b
 cola (Snoc x a) = Snoc (cola x) a

 --Ejercicio1.6
 --Rgresa la longitud.
 longitud :: SnocList a -> Int
 longitud (Snoc Mt a) = 1
 longitud (Snoc x a) = 1 + longitud x
