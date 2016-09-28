data Punto = Punto2D Float Float deriving(Show)
data Figura = Rectangulo Punto Punto | Circulo Punto Float deriving(Show)

circuloUnitario :: Figura
circuloUnitario = Circulo (Punto2D 0 0) 1

cuadradro :: Float -> Figura
cuadradro diagonal = Rectangulo (Punto2D 0 0) (Punto2D (lxDiagonal diagonal) (lxDiagonal diagonal))

-- diagonal^2 = lado^2 + lado^2
-- => lado = (diagonal/(raíz (2))
lxDiagonal :: Float -> Float
lxDiagonal diagonal = diagonal/(sqrt(2))

perimetro :: Figura -> Float
perimetro (Rectangulo (Punto2D x1 y1) (Punto2D x2 y2)) = (abs(y2-y1)*2) + (abs(x2-x1)*2)
perimetro (Circulo _ r) = (3.14159265359) * 2 * r

area :: Figura -> Float
area (Rectangulo (Punto2D x1 y1) (Punto2D x2 y2)) = abs ((y2-y1)*(x2-x1))
area (Circulo _ r) = (3.14159265359) * (r ** 2)

data ParOrdenado a b = Par a b

primero :: ParOrdenado a b -> a
primero (Par a _) = a

segundo :: ParOrdenado a b -> b
segundo (Par _ b) = b

data Lista a = ListaVacia | Agregar a ( Lista a ) deriving ( Show )

esVacia :: Lista a -> Bool
esVacia ListaVacia = True
esVacia _ = False

cabeza :: Lista a -> a
cabeza (Agregar b _) = b
--cabeza ListaVacia =

cola :: Lista a -> Lista a
cola (Agregar _ lista) = lista

concatenar :: Lista a -> Lista a -> Lista a
concatenar ListaVacia ys = ys
concatenar xs ys = Agregar (cabeza xs) (concatenar (cola xs) ys)

longitud :: Lista a -> Integer
longitud ListaVacia = 0
longitud xs = 1 + longitud(cola xs)

suma :: Lista Float -> Float
suma ListaVacia = 0
suma xs = cabeza(xs) + suma (cola xs)

posicion :: Lista a -> Float -> a
posicion xs 0 = cabeza xs
posicion xs i = posicion (cola xs) (i-1)

sonIguales :: Eq a => Lista a -> Lista a -> Bool
sonIguales xs ys | (longitud xs /= longitud ys) = False
                 | otherwise =  (cabeza xs == cabeza ys) && sonIguales (cola xs) (cola ys)

instance Eq a => Eq (Lista a) --where
--Eq sonIguales _ _ = True
--Eq sonIguales _ _ = False
---- ponele que es algo tipo quizas quien sabe esto


longitu :: [ a ] -> Integer
longitu [] = 0
longitu (x : []) = 1
longitu (x : y : []) = 2
longitu (x : y : z : []) = 3
longitu (_ : _ : _ : xs) = 3 + (longitu xs)


iniciales :: [ Char ] -> [ Char ] -> [ Char ]
iniciales nombre apellido = [n,a]
    where ( n : _ ) = head nombre : []
          ( a : _ ) = head apellido : []