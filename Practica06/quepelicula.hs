import System.IO
import System.Exit
import System.Directory
import System.IO.Unsafe  -- be careful!                                         
import System.Random
import Data.Emoji
import Data.Char

main :: IO ()
main = do
    -- Accdemos al número de intentos posibles, si son cero se finaliza el programa.
    intentos <-readFile "Intentos.txt"
    if (length intentos == 0) 
        then 
            do
                writeFile "Intentos.txt" ("aaaaa")
                putStr "Game over"
                mapM_ putStrLn ( unicodeByName "slightly_frowning_face") 
                exitSuccess
        else
            putStrLn (" ")

    --Abrimos el direcotrio donde están las películas.
    contents <- readFile "dataSetMovieEmoji.txt" 

    --Separamos en lineas dicho directorio y se almacena en una lista.
    let listPeliculas = lines contents

    --Inicio del juego ¿Qué pelícla es?
    putStrLn "¿Qué película es?"

    --Instrucciones
    putStr ("[1] Jugar")
    mapM_ putStrLn ( unicodeByName "video_game") 
    putStr ("[2] Instrucciones")
    mapM_ putStrLn ( unicodeByName "scroll") 
    putStr ("[3] Salir")
    mapM_ putStrLn ( unicodeByName "door") 

    --Almacenamos el valor pasado en consola y un archivo con instrucciones.
    instrucciones <- readFile "Instrucciones.txt" 
    eleccion <- getLine 

    --Se imprimen las instrucciones y de regresa al inicio.
    if eleccion == "2" 
        then 
            do 
                putStrLn (instrucciones)
                x <- getLine
                main
    --Se reinicia la cuenta de intentos y se sale del programa.
    else if eleccion == "3" 
        then
            do
                writeFile "Intentos.txt" ("aaaaa")
                putStrLn ("Ok ciao")
                mapM_ putStrLn ( unicodeByName "wave") 
                exitSuccess
    --Se continúa con la ejecución del programa
    else if eleccion == "1" 
        then putStrLn (" ")
    --Elije una opción correcta.
    else 
        do 
        putStrLn ("Elije una opción válida")
        main
    
    --Inicia el juego
    putStrLn ("¿Cuál es el nombre de la película? ")

    --Accede a los valores de películas guardados anteriormente y "prepara los datos", solo selecciona una aleatoriamente.
    let pelicula =  ((separaPelicula (eliminaVariosChar (listPeliculas!!(c 0 99)) ['\"','(',')'])))

    --Segundo valor guarda los emojis.
    let em = pelicula!!1

    --Primer valor guarda el nombre de la pelicula.
    let em1 = pelicula!!0

    --Muestra los emojis al usuario.
    putStrLn (toEmoji em)

    --Recibe la respuesta del usuario
    ansPel <- getLine

    --Se quitan artículos y signos de puntuación. Se eliminan espaciosy todo se pasa a minúsculas.
    let correcto = eliminaVariosChar (toLowerString (juntaCadenas (eliminaLista (splitSt ' ' em1) ["the", "The"] ))) ['.',',',':',';','_','-','?','!']
    let respuesta = eliminaVariosChar (toLowerString (juntaCadenas (eliminaLista (splitSt ' ' ansPel) ["the", "The"] ))) ['.',',',':',';','_','-','?','!']

    --Comparamos el nombre de la película con lo que ingreso el usuario y evaluamos.
    let resultado = correcto == respuesta 
    if resultado
        then mapM_ putStrLn ( unicodeByName "white_check_mark")  
        else 
            do 
                mapM_ putStrLn ( unicodeByName "x") 
                writeFile "Intentos.txt" (eliminaUnChar intentos)
                main

    --Jugar otra vez
    putStrLn "¿Otra vez? (s/n)"
    answer <- getLine
    if (answer == "s") 
        then 
            main 
        else 
            do
                writeFile "Intentos.txt" ("aaaaa")
                putStrLn "Ciao"
                exitSuccess
 

--Genera nuḿeros aleatorios en un intervalos para acceder a una película en la lista.
c :: Int -> Int -> Int
c a b = unsafePerformIO (getStdRandom (randomR (a, b)))

--Separa una lista por algún valor y regresa los trozos en otra lista.
splitSt :: (Eq a) => a -> [a] -> [[a]]
splitSt _ [] = []
splitSt separator ys = f : (splitSt separator (dropSeparator separator rest))
  where (f, rest) = break (== separator) ys

--Auxiliar de splitSt
dropSeparator :: Eq a => a ->  [a] -> [a]
dropSeparator _ [] = []
dropSeparator separator (x:xs)
    |x == separator = xs 
    |otherwise = x:xs

--Recibe una cadena sin procear y regresa una lista con su nombre y sus emojis.
separaPelicula :: String -> [String]
separaPelicula [] = []
separaPelicula x = [pel!!0] ++ [emojis]
    where
        pel = (splitSt ',' x)
        emojis = ((pel!!1))

--Elimina un char de un String.
eliminaChar :: String -> Char -> String
eliminaChar [] _ = []
eliminaChar (x:xs) a
    |x==a = eliminaChar xs a
    |otherwise = x:eliminaChar xs a

--Aplica eliminaChar a varios char.
eliminaVariosChar :: String -> [Char] -> String
eliminaVariosChar [] _ = []
eliminaVariosChar x [] = x
eliminaVariosChar x (a:as) = eliminaVariosChar (eliminaChar x a) as

--Elimina una lista que aparece en en otra lista. Se usa para quitar los artículos.
eliminaLista :: (Eq a) => [a] -> [a] -> [a]
eliminaLista [] _ = []
eliminaLista x [] = x
eliminaLista x (y:ys) = eliminaLista (filter (\a -> a /= y ) x) ys

--A partir de una lista de cadenas genera una sola cadena con la unión de sus elementos.
juntaCadenas :: [String] -> String
juntaCadenas [] = []
juntaCadenas (x:xs) = x ++ juntaCadenas xs

--Cambia el \\ por \ para que los emojis puedan ser interpretados.
toEmoji :: String -> String
toEmoji s = read $ "\"" ++ s ++ "\""

--Recibeun string y convierte todos sus char en minúsculas.
toLowerString :: [Char] -> [Char]
toLowerString str = [ toLower x | x <- str]

--Elimina la cabeza de un string
eliminaUnChar :: String -> String
eliminaUnChar [] = []
eliminaUnChar (x:xs) = xs