data Arbol = Hoja Integer | Rama Arbol Integer Arbol deriving (Eq, Show)
data Dir = Der | Izq

-- TEST
arbolito = Rama (Hoja 2) 5 (Rama (Hoja 1) 10 (Rama (Hoja 1) 10 (Hoja 0)))
arboleto = Rama (Rama (Hoja 1) 10 (Hoja 0)) 5 (Rama (Hoja 1) 10 (Hoja 0))
altoarbol = Rama (Rama (Hoja 1) 10 (Rama (Rama (Hoja 1) 10 (Hoja 0)) 5 (Rama (arboleto) 10 (Hoja 0)))) 5 (Rama (Hoja 1) 10 (Hoja 0))

esHoja :: Arbol -> Bool
esHoja (Hoja _) = True
esHoja _ = False

sumaNodos :: Arbol -> Integer
sumaNodos (Rama a x b) = sumaNodos a + x + sumaNodos b
sumaNodos (Hoja x) = x

altura :: Arbol -> Integer
altura (Rama (Hoja _) _ r )= 1 + altura r
altura (Rama r _ (Hoja _) )= 1 + altura r
altura (Rama (Rama a _ b) _ (Rama c _ d)) | (altura (Rama a 0 b)) > (altura (Rama c 0 d)) = altura (Rama a 0 b)
                                          | (altura (Rama a 0 b)) < (altura (Rama c 0 d)) = altura (Rama c 0 d)
altura (Hoja _) = 1

-- solucion de pizarrón
-- altura hoja _ = 1
-- altura (Rama a1 _ a2) = 1 + max (altura a1) (altura a2)

pertenece :: Integer -> Arbol -> Bool
pertenece x (Rama a h b) | h == x = True
                         | otherwise = (pertenece x a) || (pertenece x b)
pertenece x (Hoja h) = x == h

movete :: Dir -> Arbol -> Arbol
movete Izq (Rama l _ r) = l
movete Der (Rama l _ r) = r

busqueda :: [Dir] -> Arbol -> Integer
busqueda [] (Hoja x) = x
busqueda [] (Rama _ x _) = x
busqueda (Der:dirs) (Rama l _ r) = busqueda dirs r
busqueda (Izq:dirs) (Rama l _ r) = busqueda dirs l