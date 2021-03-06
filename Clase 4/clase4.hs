productoria :: [Integer] -> Integer
productoria lista | length lista == 0 = 1
                  | otherwise = head lista * productoria (tail lista)

reverso :: [a] -> [a]
reverso lista | length lista == 0 = []
              | otherwise = reverso (tail lista) ++ [head lista]

pertenece :: Integer -> [Integer] -> Bool
pertenece x lista | length lista == 0 = False
                  | head lista == x = True
                  | otherwise = pertenece x (tail lista)

hayRepetidos :: [Integer] -> Bool
hayRepetidos lista | length lista == 0 = False
                   | pertenece (head lista) (tail lista) = True
                   | otherwise = hayRepetidos (tail lista)

quitar :: Integer -> [Integer] -> [Integer]
quitar x lista | length lista == 0 = []
               | x == head lista =  tail lista
               | otherwise = (head lista) : (quitar x (tail lista))

maximo :: [Integer] -> Integer
maximo lista | length lista == 0 = 0
             | length lista == 1 = head lista
             | head lista < head (tail lista) = maximo (tail lista)
             | head lista >= head (tail lista) = maximo ( (head lista) : (tail (tail lista) ) )

ordenar :: [Integer] -> [Integer]
ordenar lista | length lista == 0 = []
              | length lista == 1 = lista
              | otherwise = (maximo lista : ordenar (quitar (maximo lista) lista))

sumaList :: [Integer] -> [Integer] -> [Integer]
sumaList xs ys | length xs /= length ys = []
               | length xs == 0 = []
               | otherwise = (head xs + head ys) : sumaList (tail xs) (tail ys)

eliminarRep :: [Integer] -> [Integer]
eliminarRep lista | length lista == 0 = []
                  | pertenece (head lista) (tail lista) = eliminarRep (tail lista)
                  | otherwise = head lista : eliminarRep (tail lista)

prodInterno :: [Float] -> [Float] -> Float
prodInterno xs ys | length xs == 0 = 0
                  | otherwise = (head xs)*(head ys) + prodInterno (tail xs) (tail ys)

noTieneDivisoresHasta :: Integer -> Integer -> Bool
noTieneDivisoresHasta m n | m == 2 && (mod n 2 /= 0) = True
                          | mod n m == 0 || mod n 2 == 0 = False
                          | otherwise = noTieneDivisoresHasta (m-1) n

esPrimo :: Integer -> Bool
esPrimo n | n == 1 = True
          | n == 2 = True
          | noTieneDivisoresHasta (n-1) n = True
          | otherwise = False
