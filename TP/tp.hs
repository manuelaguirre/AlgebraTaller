-- A <=> T
-- C <=> G
-- Sí mRNA => T = U (reemplaza las Ts por Us)
-- mRNA está formado por secuencia de codones
-- AUG abre la secuencia y cierra la secuencia UAA, UAG, UGA
-- cada codón tiene asociada una proteína dependiendo del orden de las letras (Ver tabla en PDF)

------ TESTS ------

cadenaDNA1 = [A,T,A,C,T,C,G,T,A,A,T,T,C,A,C,T,C,C]  -- >        [[Ser,Ile,Lys]]
cadenaDNA2 = [T,T,A,A,T,A,C,G,A,C,A,T,A,A,T,T,A,T]  -- >        [[Leu,Tyr],[Ser,Tyr]]
cadenaRNA1 = [A,U,G,A,A,A,A,U,G,A,A,A,U,A,A,A,A,A,U,A,A] -- >   [[Lys,Met,Lys],[Lys]]
---- END TESTS ----

data BaseNucleotidica = A | C | G | T | U deriving (Eq,Show)
type CadenaDNA = [BaseNucleotidica]
type CadenaRNA = [BaseNucleotidica]
type Codon = (BaseNucleotidica, BaseNucleotidica, BaseNucleotidica)
data Aminoacido = Phe | Ser | Tyr | Cys | Leu | Trp | Pro | His | Arg | Gln | Ile | Thr | Asn | Lys | Met | Val | Ala | Asp | Gly | Glu deriving Show
type Proteina = [Aminoacido]

-- Implementar

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
transcribir cd = reemplazarTsporUs ( complementarCadenaDNA cd )

----------------- seccion fea ------------------------

traducirListaDeCodonesAAminoacidos :: CadenaRNA -> Proteina
traducirListaDeCodonesAAminoacidos (c1:c2:c3:cs) = [traducirCodonAAminoacido (c1,c2,c3)] ++ traducirListaDeCodonesAAminoacidos cs
traducirListaDeCodonesAAminoacidos _ = []

principioRNA :: CadenaRNA -> CadenaRNA
principioRNA (A : U : G : rna) | (sincronizaConCodonDeFin rna) == True = rna
principioRNA ( _ : rna ) | length rna > 2 = principioRNA rna
                         | otherwise = []

-- Funciona cuando va de 3 en 3 (esto no debería importar ya que se llama después de haber llamado a
-- 'sincronizaConCodonDeFin' en principioRNA, asegurándonos que va a tener final.
finalRNA :: CadenaRNA -> CadenaRNA
finalRNA [] = []
finalRNA (b1 : []) = []
finalRNA (b1 : b2 : []) = []
finalRNA (U : A : G : _) = []
finalRNA (U : A : A : _) = []
finalRNA (U : G : A : _) = []
finalRNA (b1 : b2 : b3 : rna) = [b1,b2,b3] ++ finalRNA (rna)

-- obtenerRNAparaCodificar [G, G, A, A, A, U, G, T, T, T, T, T, T,  U, G, A] FUNCIONÓ
obtenerRNAparaCodificar :: CadenaRNA -> CadenaRNA
obtenerRNAparaCodificar rna = finalRNA (principioRNA rna)

obtenerProteinaDeRNA :: CadenaRNA -> [Proteina]
obtenerProteinaDeRNA rna = [traducirListaDeCodonesAAminoacidos (obtenerRNAparaCodificar  rna)]


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
obtenerProteinas = error "Implementar"
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