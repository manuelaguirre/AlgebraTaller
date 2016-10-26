-- A <=> T
-- C <=> G
-- Sí mRNA => T = U (reemplaza las Ts por Us)
-- mRNA está formado por secuencia de codones
-- AUG abre la secuencia y cierra la secuencia UAA, UAG, UGA
-- cada codón tiene asociada una proteína dependiendo del orden de las letras (Ver tabla en PDF)

------ TESTS ------

cadenaDNA = [T,A,C,T,C,G,T,A,A,T,T,C,A,C,T]

---- END TESTS ----


data BaseNucleotidica = A | C | G | T | U deriving (Eq,Show)
type CadenaDNA = [BaseNucleotidica]
type CadenaRNA = [BaseNucleotidica]
type Codon = (BaseNucleotidica, BaseNucleotidica, BaseNucleotidica)
data Aminoacido = Phe | Ser | Tyr | Cys | Leu | Trp | Pro | His | Arg | Gln | Ile | Thr | Asn | Lys | Met | Val | Ala | Asp | Gly | Glu deriving Show
type Proteina = [Aminoacido]

-- Tenemos
traducirCodonAAminoacido:: Codon -> Aminoacido
traducirCodonAAminoacido c = Phe

-- Implementar

complementarBase :: BaseNucleotidica -> BaseNucleotidica
complementarBase A = T
complementarBase T = A -- Comentaron que no teníamos que poner la U acá
complementarBase C = G
complementarBase G = C

reemplazarTsporUs :: CadenaDNA -> CadenaRNA
reemplazarTsporUs [] = []
reemplazarTsporUs (T:bs) = U : reemplazarTsporUs bs
reemplazarTsporUs (b:bs) = b : reemplazarTsporUs bs
-- Dada una base nucleotídica devuelve su base complementaria.

complementarCadenaDNA :: CadenaDNA -> CadenaDNA
complementarCadenaDNA [] = []
complementarCadenaDNA (b : bs) = (complementarBase b) : complementarCadenaDNA bs

obtenerCadenaReverseDNA :: CadenaDNA -> CadenaDNA
-- Dada una cadena de DNA devuelve su cadena reverse.
obtenerCadenaReverseDNA [] = []
obtenerCadenaReverseDNA (b : bs) = obtenerCadenaReverseDNA bs ++ [b]

transcribir :: CadenaDNA -> CadenaRNA
-- Dada una cadena de DNA devuelve su transcripción a RNA.
transcribir cd = reemplazarTsporUs ( complementarCadenaDNA cd )

----------------- seccion fea ------------------------

traducirListaDeCodonesAAminoacidos :: CadenaRNA -> Proteina
traducirListaDeCodonesAAminoacidos [] = []
traducirListaDeCodonesAAminoacidos (c1:c2:c3:cs) = [traducirCodonAAminoacido (c1,c2,c3)] ++ traducirListaDeCodonesAAminoacidos cs

principioRNA :: CadenaRNA -> CadenaRNA
principioRNA (A : U : G : rna) | (sincronizaConCodonDeFin rna) == True = rna
principioRNA ( _ : rna ) = principioRNA rna

-- Funciona cuando va de 3 en 3
finalRNA :: CadenaRNA -> CadenaRNA
finalRNA (b1 : rna) | length rna < 3 = []
finalRNA (U : A : G : _) = []
finalRNA (U : A : A : _) = []
finalRNA (U : G : A : _) = []
finalRNA (b1 : b2 : b3 : rna) = [b1,b2,b3] ++ finalRNA (rna)

-- obtenerRNAparaCodificar [G, G, A, A, A, U, G, T, T, T, T, T, T,  U, G, A] FUNCIONÓ
obtenerRNAparaCodificar :: CadenaRNA -> CadenaRNA
obtenerRNAparaCodificar rna = finalRNA (principioRNA rna)

obtenerProteinaDeRNA :: CadenaRNA -> [Proteina]
obtenerProteinaDeRNA _ = []
--Devuelve las secuencia proteica codificada por una cadena de RNA dada.
--obtenerProteinaDeRNA ( A : U : G : [a,b] : U : G : A) = [] -- falta si hay uno, o ninguno en el medio
--obtenerProteinaDeRNA ( A : U : G : rna : U : G : A) = traducirListaDeCodonesAAminoacidos rna
-- 1. Encontrar inici
-- 2. Encontrar final

sincronizaConCodonDeFin :: CadenaRNA -> Bool
sincronizaConCodonDeFin _ = True
--Devuleve True si existe un codón de fin a una distancia múltiplo de 3 del inicio de una cadena de
--RNA dada y False en caso contrario.
--sincronizaConCodonDeFin ( A : U : G : rna : U : G : A) = mod (length rna) 3 == 0

obtenerProteinas :: CadenaDNA -> [Proteina]
obtenerProteinas cd = []
-- Dada una cadena de DNA devuelve una lista de las proteinas codificandas. En caso de que una
-- secuencia codifique más de una proteína, todas deben estar presentes en la lista que devuelva la
-- función. El orden en que deben aparecer es: 1) las codificadas por la secuencia original, 2) por la
-- secuencia reversa, 3) por la secuencia complementaria, 4) por la secuencia complementaria reversa.
-- Algunas secuencias válidas de DNA no codifican proteínas; para esos casos la función debe devolver
-- la lista vacía. OBS: Usar las dos anteriores.