-- Cuidado, puede llegar a matar la compu, un (2, 4) termina siendo un 2^16
superPotencia :: (Integer, Integer) -> Integer
superPotencia (x, 0) = 1
superPotencia (x, y) = x ^ (superPotencia (x, y-1))

-- No la pude testear, no me devolvió nada.
ackermann :: (Integer, Integer) -> Integer
ackermann (0, n) = n + 1
ackermann (m, 0) = ackermann (m - 1, 1)
ackermann (m, n) = ackermann (m - 1, ackermann (m, n - 1))

-- No puedo creer, alta recursiva, me amo.
substring :: String -> Integer -> Integer -> String
substring palabra 0 0 = []
substring palabra 0 cantLetras = head palabra : substring (tail palabra) 0 (cantLetras - 1)
substring palabra index cantLetras = substring (tail palabra) (index-1) cantLetras

replicar :: String -> Integer -> String
replicar palabra 0 = []
replicar palabra cantVeces = head palabra : replicar palabra (cantVeces-1)