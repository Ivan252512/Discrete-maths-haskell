module Relacion where

  -- Sinónimo para representar relaciones binarias.
  type RelacionB a b = [(a,b)]

  -- OPERACIONES ENTRE RELACIONES

  --Ejercicio 1.1
  -- Agrega en un nuevo conjunto a ambos conjuntos de manera recursiva.
  unionR :: Eq a => Eq b => RelacionB a b -> RelacionB a b -> RelacionB a b
  unionR [] [] = []
  unionR [] (x:xs) = x:unionR [] xs
  unionR (x:xs) [] = x:unionR xs []
  unionR (x:xs) (y:ys)
    |x==y = x:unionR xs ys
    |otherwise = x:(unionR xs ys) ++ [y]

  --Ejercicio 1.2
  --Intersección de dos conjuntos.
  interseccion :: Eq a => Eq b => RelacionB a b -> RelacionB a b -> RelacionB a b
  interseccion [] [] = []
  interseccion [] _ = []
  interseccion _ [] = []
  interseccion (x:xs) (y:ys)
    |x==y = x:interseccion xs ys
    |otherwise = interseccion xs ys

  --Ejercicio 1.3
  --Si el elemento del segundo conjunto está en el primero no o agrega, sino sí.
  diferencia :: Eq a => Eq b => RelacionB a b -> RelacionB a b -> RelacionB a b
  diferencia [] [] = []
  diferencia [] _ = []
  diferencia x [] = x
  diferencia (x:xs) (y:ys)
    |x==y = diferencia xs ys
    |otherwise = x:diferencia xs ys

  --Ejercicio 1.4
  --Crea una relación entre dos listas.
  productoCartesiano :: [a] -> [b] -> RelacionB a b
  productoCartesiano [] [] = []
  productoCartesiano _ [] = []
  productoCartesiano [] _ = []
  productoCartesiano (x:xs) (y:ys) = (x,y):(productoCartesiano [x] ys) ++ productoCartesiano xs (y:ys)

  -- PROPIEDADES

  --Ejercicio 2.1
  simetrica :: Eq a => RelacionB a a -> Bool
  simetrica [] = True
  simetrica ((a,b):xs) = sim1 && simetrica (diferencia xs [(b,a)])
    where
      --Si (a,b) están en la relación tiene que estar al menos una vez (b,a)
      sim1 =(length (filter ( \x -> x==(b,a) ) ((a,b):xs))) > 0


  --Ejercicio 2.2
  antisimetrica :: Eq a => RelacionB a a -> Bool
  antisimetrica [] = True 
  antisimetrica ((a,b):xs) = antsim1 && antisimetrica (diferencia xs [(a,b)])
      where
        sim1 = filter ( \x -> x==(b,a) ) ((a,b):xs)
        --Solo se puede si ambos elementos son iguales.
        antsim1 = ( length (filter (\(c,d) -> c /= d) sim1)) == 0
        

  --Ejercicio 2.3
  reflexiva :: Eq a => [a] -> RelacionB a a -> Bool
  reflexiva [] _ = True 
  reflexiva _ [] = False
  reflexiva (x:xs) r = ref1 && reflexiva xs r
    where
      --El elemento debe serel mismo
      ref1 = length (filter (\rx -> rx == (x,x)) r) > 0  

  --Ejercicio 2.4 
  antireflexiva :: Eq a => RelacionB a a -> Bool
  antireflexiva [] = True
  antireflexiva ((a,b):xs) = aref1 && antireflexiva xs 
    where
      --No debe haber más de cero elementos que sean reflexivos.
      aref1 = length (filter (\rx -> rx == (a,a) || rx == (b,b)) ((a,b):xs)) == 0  

  --Ejercicio 2.5
  asimetrica :: Eq a => RelacionB a a -> Bool
  asimetrica [] = True
  asimetrica ((a,b):xs) = asim1 && asimetrica xs
    where
      asim1 =(length (filter ( \x -> x==(b,a) ) ((a,b):xs))) == 0

  -- OPERACIONES SOBRE RELACIONES

  --Ejercicio 3.1
  inversa :: Eq a => RelacionB a a -> RelacionB a a
  inversa [] = []
  inversa ((a,b):xs) = (b,a):inversa xs 

  --Ejercicio 3.2
  complemento :: Eq a =>[a] -> RelacionB a a -> RelacionB a a
  complemento c x = diferencia (productoCartesiano c c) x