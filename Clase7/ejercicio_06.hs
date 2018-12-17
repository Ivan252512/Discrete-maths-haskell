
data AE = Var String | Const Int | Op Operacion AE AE deriving (Show)

data Operacion = Suma | Resta | Mult | Div deriving (Show)

type Estado = [(String,AE)]

opera :: Operacion -> (Int -> Int -> Int)
opera Suma = (\x y -> x + y)
opera Resta = (\x y -> x - y)
opera Mult = (\x y -> x * y)
opera Div = (\x y -> div x y)

busca :: String -> Estado -> AE
busca s [] = error "Es lista vacia"
busca s (x:xs)
  |fst x == s = snd x
  |otherwise = busca s (xs)

eval :: AE -> Estado -> Int
eval (Const x) [] = x
eval (Op o (Var v) (Const x)) l = opera o x c
  where
    (Const c) = busca v l
