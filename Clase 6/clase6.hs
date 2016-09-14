yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _ = False

oLogico :: Bool -> Bool -> Bool
oLogico False False = False
oLogico _ _ = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

-- Falso, implica verdadero, es verdadero --> implica False _ = True

sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana x = x + sumaGaussiana (x-1)

algunoEsCero :: (Integer, Integer, Integer) -> Bool
algunoEsCero (0,_,_) = True
algunoEsCero (_,0,_) = True
algunoEsCero (_,_,0) = True
algunoEsCero (_,_,_) = False

productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x1, y1) (x2, y2) = (x1*x2) + (y1*y2)

data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving(Enum, Eq, Ord, Show)

esFinde :: Dia -> Bool
esFinde Sabado = True
esFinde Domingo = True
esFinde _ = False

esDiaHabil :: Dia -> Bool
esDiaHabil d = not (esFinde d)

type Posicion = (Integer, Integer)
data Direccion = Norte | Sur | Este | Oeste deriving(Show)
type Tortuga = (Posicion, Direccion)

arrancar :: Tortuga
arrancar = ((0,0), Sur)

girarDerecha :: Tortuga -> Tortuga
girarDerecha (t, Norte) = (t, Este)
girarDerecha (t, Este) = (t, Sur)
girarDerecha (t, Sur) = (t, Oeste)
girarDerecha (t, Oeste) = (t, Norte)

avanzar :: Tortuga -> Integer -> Tortuga
avanzar ((p1, p2), Norte) d = ((p1, p2+d), Norte)
avanzar ((p1, p2), Sur) d = ((p1, p2-d), Sur)
avanzar ((p1, p2), Este) d = ((p1+d, p2), Este)
avanzar ((p1, p2), Oeste) d = ((p1-d, p2), Oeste)