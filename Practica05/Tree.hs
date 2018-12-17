module Tree where

 -- Tipo de dato Algebraico para definir Árboles Binarios
 data BinaryTree a = Void
                   | Node (BinaryTree a) a (BinaryTree a)
                   deriving (Eq,Ord,Show)

 --Ejercicio 2.1
 --Agrega elementos al arbol.
 addTree :: (Ord a) => BinaryTree a -> a -> BinaryTree a
 addTree Void a = Node (Void) a (Void) 
 addTree (Node (izq) n (der)) a
    | a <= n =  Node (addTree izq a) n (der)
    |otherwise = Node (izq) n (addTree der a)

 --Ejercicio 2.2
 --Recorrido inorder.
 inorder :: BinaryTree a -> [a]
 inorder Void = []
 inorder (Node (izq) a (der)) = (inorder (izq) ++ [a] ++inorder (der))

 --Ejercicio 2.3
 --Recorrido Preorder.
 preorder :: BinaryTree a -> [a]
 preorder Void = []
 preorder (Node (izq) a (der)) = a : (preorder (izq) ++ preorder (der))

 --Ejercicio 2.4
 --Recorrido postorder.
 postorder :: BinaryTree a -> [a]
 postorder Void = []
 postorder (Node (izq) a (der)) = (postorder (izq) ++ postorder (der)) ++ [a]

 --Ejercicio 2.5
 maximo :: (Ord a) => BinaryTree a -> a
 maximo a = last (inorder a)

 --Ejercicio 2.6
 minimo :: (Ord a) => BinaryTree a -> a
 minimo a = c
    where
        c:d = inorder a

 --Ejercicio 2.7
 --Regresa true si el elemento está en el árbol false en caso contrario.
 busca :: (Ord a) => a -> BinaryTree a -> Bool
 busca _ Void = False
 busca a (Node (Void) b (Void))  = compare a b == EQ
 busca a (Node (left) b (right))  
    |compare a b == EQ = True
    |compare a b == LT = busca a left
    |compare a b == GT = busca a right

