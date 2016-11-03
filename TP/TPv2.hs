module TPv2 where

-- Integarntes: Manuel Aguirre, Max Schulkin, Alejo Amiras
-- Turno: Miércoles (Tarde)

------ TESTS ------
cadenaDNA1 = [A,T,A,C,T,C,G,T,A,A,T,T,C,A,C,T,C,C]          -- >     [[Ser,Ile,Lys]]
cadenaRNA1 = transcribir cadenaDNA1
cadenaDNA2 = [T,T,A,A,T,A,C,G,A,C,A,T,A,A,T,T,A,T]          -- >     [[Leu,Tyr],[Ser,Tyr]]
cadenaRNA2 = transcribir cadenaDNA2
cadenaDNA3 = [G,C,C,T,T,G,A,T,A,T,G,G,A,G,A,A,C,T,C,A,T,T]  -- >     []
cadenaRNA3 = transcribir cadenaDNA3
cadenaRNA4 = [A,U,G,A,A,A,A,U,G,A,A,A,U,A,A,A,A,A,U,A,A]    -- >     [[Lys,Met,Lys],[Lys]]
pest10 = [A,U,G,U,G,A,A,U,G,U,U,U,U,G,A]                    -- >     [[Phe]]

-- El que no funciona (hay algo raro cuando se evalúa la cadena reverse) --
cb = [A,T,G,T,C,G,A,T,C,A,G,C,A,T,C,G,A,C,T,A,C,G,A,C,T,A,C,G,A,C,G,A,C,T,A,C,A,G,C,A,T,C,A,G,T,C,A,G,T,C,A,T,C,A,G,C,A,T,C,A,G,T,A,C,C,T,A,C,T,A,C,T,C,A,A,A,C,A,C,C,A,C,A,G,A,T,A,T,T,T,G,A,G,A,T,T,A,A,G,T,A,A,T,A,G,G,A,A,G,T,C,C,C,C,A,T,A,G,A]
mb = [[Leu,Met,Leu,Leu,Met,Ser],[Leu,Leu,Met,Ser],[Ser],[Met,Ser,Leu,Trp,Cys,Leu],[Ser,Leu,Trp,Cys,Leu],[Ser,Ile,Ser,Ile,Asp,Tyr,Asp,Tyr,Asp,Asp,Tyr,Ser,Ile,Ser,Gln,Ser,Ser,Ala,Ser,Val,Pro,Thr,Thr,Gln,Thr,Pro,Gln,Ile,Phe,Glu,Ile,Lys],[Gly,Thr,Ser,Tyr,Tyr,Leu,Ile,Ser,Asn,Ile,Cys,Gly,Val],[Leu,Met,Thr,Asp],[Thr,Asp],[Leu],[Asn],[Thr,Thr,Thr,Thr,Asp]]
---- END TESTS ----

data BaseNucleotidica = A | C | G | T | U deriving (Eq,Show)
type CadenaDNA = [BaseNucleotidica]
type CadenaRNA = [BaseNucleotidica]
type Codon = (BaseNucleotidica, BaseNucleotidica, BaseNucleotidica)
data Aminoacido = Phe | Ser | Tyr | Cys | Leu | Trp | Pro | His | Arg | Gln | Ile | Thr | Asn | Lys | Met | Val | Ala | Asp | Gly | Glu deriving (Eq, Show)
type Proteina = [Aminoacido]

complementarBase :: BaseNucleotidica -> BaseNucleotidica
complementarBase A = T
complementarBase T = A
complementarBase C = G
complementarBase G = C

reemplazarTsporUs :: CadenaDNA -> CadenaRNA
reemplazarTsporUs [] = []
reemplazarTsporUs (T:bs) = U : reemplazarTsporUs bs
reemplazarTsporUs (b:bs) = b : reemplazarTsporUs bs

complementarCadenaDNA :: CadenaDNA -> CadenaDNA
complementarCadenaDNA [] = []
complementarCadenaDNA (b : bs) = (complementarBase b) : complementarCadenaDNA bs

obtenerCadenaReverseDNA :: CadenaDNA -> CadenaDNA
obtenerCadenaReverseDNA [] = []
obtenerCadenaReverseDNA (b : bs) = obtenerCadenaReverseDNA bs ++ [b]

transcribir :: CadenaDNA -> CadenaRNA
transcribir dna = reemplazarTsporUs ( complementarCadenaDNA dna )

obtenerProteinaDeDNA :: CadenaDNA -> [Proteina]
obtenerProteinaDeDNA dna = obtenerProteinaDeRNA (transcribir dna)

obtenerProteinaDeRNA :: CadenaRNA -> [Proteina]
obtenerProteinaDeRNA rna  = codificarRNA (rna)

----------------- seccion fea ------------------------

codificarRNA :: CadenaRNA -> [Proteina]
codificarRNA [] = []
codificarRNA (b1:[]) = []
codificarRNA (b1:b2:[]) = []
codificarRNA (A:U:G:rna) | (sincronizaConCodonDeFin rna) && length (codificarCadena rna) > 0 = [codificarCadena (rna)] ++ codificarRNA (rna)
                         | otherwise = codificarRNA(rna)
codificarRNA (b1:rna) = codificarRNA(rna)

codificarCadena :: CadenaRNA -> Proteina
codificarCadena (b1 : []) = []
codificarCadena (b1 : b2 : []) = []
codificarCadena (U : A : G : _) = []
codificarCadena (U : A : A : _) = []
codificarCadena (U : G : A : _) = []
codificarCadena (b1:b2:b3:rna) = [traducirCodonAAminoacido (b1,b2,b3)] ++ codificarCadena (rna)


--
sincronizaConCodonDeFin :: CadenaRNA -> Bool
sincronizaConCodonDeFin [] = False
sincronizaConCodonDeFin (b1 : []) = False
sincronizaConCodonDeFin (b1 : b2 : []) = False
sincronizaConCodonDeFin (U : A : G : _) = True
sincronizaConCodonDeFin (U : A : A : _) = True
sincronizaConCodonDeFin (U : G : A : _) = True
sincronizaConCodonDeFin (b1 : b2 : b3 : rna) = False || sincronizaConCodonDeFin (rna)

obtenerProteinas :: CadenaDNA -> [Proteina]
obtenerProteinas dna = (obtenerProteinaDeDNA dna) ++ (obtenerProteinaDeDNA (obtenerCadenaReverseDNA dna)) ++ obtenerProteinaDeDNA (complementarCadenaDNA dna) ++ obtenerProteinaDeDNA (obtenerCadenaReverseDNA (complementarCadenaDNA dna))
-- Dada una cadena de DNA devuelve una lista de las proteinas codificandas. En caso de que una
-- secuencia codifique más de una proteína, todas deben estar presentes en la lista que devuelva la
-- función. El orden en que deben aparecer es: 1) las codificadas por la secuencia original, 2) por la
-- secuencia reversa, 3) por la secuencia complementaria, 4) por la secuencia complementaria reversa.
-- Algunas secuencias válidas de DNA no codifican proteínas; para esos casos la función debe devolver
-- la lista vacía. OBS: Usar las dos anteriores.







-- Funcion que dado un codon devuelve el correspondiente aminoacido
traducirCodonAAminoacido:: Codon -> Aminoacido
traducirCodonAAminoacido (A, A, A) = Lys
traducirCodonAAminoacido (A, A, U) = Asn
traducirCodonAAminoacido (A, A, C) = Asn
traducirCodonAAminoacido (A, A, G) = Lys
traducirCodonAAminoacido (A, U, A) = Ile
traducirCodonAAminoacido (A, U, U) = Ile
traducirCodonAAminoacido (A, U, C) = Ile
traducirCodonAAminoacido (A, U, G) = Met
traducirCodonAAminoacido (A, C, A) = Thr
traducirCodonAAminoacido (A, C, U) = Thr
traducirCodonAAminoacido (A, C, C) = Thr
traducirCodonAAminoacido (A, C, G) = Thr
traducirCodonAAminoacido (A, G, A) = Arg
traducirCodonAAminoacido (A, G, U) = Ser
traducirCodonAAminoacido (A, G, C) = Ser
traducirCodonAAminoacido (A, G, G) = Arg
traducirCodonAAminoacido (U, A, U) = Tyr
traducirCodonAAminoacido (U, A, C) = Tyr
traducirCodonAAminoacido (U, U, A) = Leu
traducirCodonAAminoacido (U, U, U) = Phe
traducirCodonAAminoacido (U, U, C) = Phe
traducirCodonAAminoacido (U, U, G) = Leu
traducirCodonAAminoacido (U, C, A) = Ser
traducirCodonAAminoacido (U, C, U) = Ser
traducirCodonAAminoacido (U, C, C) = Ser
traducirCodonAAminoacido (U, C, G) = Ser
traducirCodonAAminoacido (U, G, U) = Cys
traducirCodonAAminoacido (U, G, C) = Cys
traducirCodonAAminoacido (U, G, G) = Trp
traducirCodonAAminoacido (C, A, A) = Gln
traducirCodonAAminoacido (C, A, U) = His
traducirCodonAAminoacido (C, A, C) = His
traducirCodonAAminoacido (C, A, G) = Gln
traducirCodonAAminoacido (C, U, A) = Leu
traducirCodonAAminoacido (C, U, U) = Leu
traducirCodonAAminoacido (C, U, C) = Leu
traducirCodonAAminoacido (C, U, G) = Leu
traducirCodonAAminoacido (C, C, A) = Pro
traducirCodonAAminoacido (C, C, U) = Pro
traducirCodonAAminoacido (C, C, C) = Pro
traducirCodonAAminoacido (C, C, G) = Pro
traducirCodonAAminoacido (C, G, A) = Arg
traducirCodonAAminoacido (C, G, U) = Arg
traducirCodonAAminoacido (C, G, C) = Arg
traducirCodonAAminoacido (C, G, G) = Arg
traducirCodonAAminoacido (G, A, A) = Glu
traducirCodonAAminoacido (G, A, U) = Asp
traducirCodonAAminoacido (G, A, C) = Asp
traducirCodonAAminoacido (G, A, G) = Glu
traducirCodonAAminoacido (G, U, A) = Val
traducirCodonAAminoacido (G, U, U) = Val
traducirCodonAAminoacido (G, U, C) = Val
traducirCodonAAminoacido (G, U, G) = Val
traducirCodonAAminoacido (G, C, A) = Ala
traducirCodonAAminoacido (G, C, U) = Ala
traducirCodonAAminoacido (G, C, C) = Ala
traducirCodonAAminoacido (G, C, G) = Ala
traducirCodonAAminoacido (G, G, A) = Gly
traducirCodonAAminoacido (G, G, U) = Gly
traducirCodonAAminoacido (G, G, C) = Gly
traducirCodonAAminoacido (G, G, G) = Gly