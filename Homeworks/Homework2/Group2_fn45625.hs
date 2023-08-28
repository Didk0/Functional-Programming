module Group2_fn45625 where

data Base = C | G | A | T
    deriving (Eq, Show)

data GeneticCode = GeneticCode [Base]
    deriving (Eq, Show)

data Codon = Codon Base Base Base
    deriving (Eq, Show)

data Aminoacid = Aminoacid Int
    deriving (Eq, Ord, Show)

type CodingTable = [(Codon, Aminoacid)]

type Gene = [Codon]

type Protein = [Aminoacid]

-------------------------------------------------------------------------------------
-- task 1

geneticCodeToCodons :: GeneticCode -> [Codon]
geneticCodeToCodons (GeneticCode []) = []
geneticCodeToCodons (GeneticCode [x]) = []
geneticCodeToCodons (GeneticCode [x,y]) = []
geneticCodeToCodons (GeneticCode (x:y:z:xs)) = Codon x y z : geneticCodeToCodons (GeneticCode xs)

isStopCodon :: Codon -> CodingTable -> Bool
isStopCodon codon table = codon `notElem` [fst pair | pair <- table]

takeAllGenes :: [Codon] -> CodingTable -> [Gene]
takeAllGenes [] table = []
takeAllGenes codons table = helper [] [] codons where
    helper :: [Gene] -> Gene -> [Codon] -> [Gene]
    helper res curr cods
        | null cods = if not $ null curr then res ++ [curr] else res
        | isStopCodon (head cods) table = if not $ null curr then helper (curr:res) [] (tail cods) else helper res [] (tail cods)
        | otherwise = helper res (curr ++ [head cods]) (tail cods)
        
takeCodonBase :: Codon -> [Base]
takeCodonBase (Codon x y z) = [x,y,z]

codeCodonToAminoacid :: Codon -> CodingTable -> Aminoacid
codeCodonToAminoacid codon table = if null lst
                                    then Aminoacid (-1)
                                    else head lst where
                                        lst = [snd pair | pair <- table, takeCodonBase (fst pair) == takeCodonBase codon]

codeGeneToProtein :: Gene -> CodingTable -> Protein
codeGeneToProtein gene table = [codeCodonToAminoacid codon table | codon <- gene, codeCodonToAminoacid codon table /= Aminoacid (-1)] 

uniques :: Eq a => [a] -> [a]
uniques [] = []
uniques (x:xs) = if x `elem` xs then uniques xs else x : uniques xs

hasSameProteins :: CodingTable -> GeneticCode -> Bool
hasSameProteins table geneCode = length (uniques [codeGeneToProtein gene table | gene <- allSubGenes]) /= length allSubGenes where
    allSubGenes = takeAllGenes (geneticCodeToCodons geneCode) table

---------------------------------------------------------------------------------------
-- task 2

allCodonOfSameAminoacid :: Gene -> Codon -> CodingTable -> [Codon]
allCodonOfSameAminoacid gene codon table = [cod | cod <- gene, codeCodonToAminoacid codon table == codeCodonToAminoacid cod table]

getBaseDiff :: Codon -> Codon -> Int 
getBaseDiff (Codon x y z) (Codon u v t)
    | (x /= u && y == v && z == t) || (x == u && y /= v && z == t) || (x == u && y == v && z /= t) = 1
    | (x /= u && y /= v && z == t) || (x == u && y /= v && z /= t) || (x /= u && y == v && z /= t) = 2
    | x /= u && y /= v && z /= t = 3
    | otherwise = 0

maxGeneDiffOneCod :: Gene -> CodingTable -> Codon -> Int
maxGeneDiffOneCod gene table codon = if not $ null [getBaseDiff codon cod | cod <- allCodonOfSameAminoacid gene codon table]
                                            then maximum [getBaseDiff codon cod | cod <- allCodonOfSameAminoacid gene codon table]
                                            else 0

maxGeneDiff :: Gene -> CodingTable -> Int
maxGeneDiff gene table = sum [maxGeneDiffOneCod gene table codon | codon <- gene]

maxGeneCodeMutations :: GeneticCode -> CodingTable -> Int
maxGeneCodeMutations geneCode table = sum [maxGeneDiff gene table | gene <- takeAllGenes (geneticCodeToCodons geneCode) table]

takeAllStopCodons :: CodingTable -> GeneticCode -> [Codon]
takeAllStopCodons table geneCode = [codon | codon <- geneticCodeToCodons geneCode, isStopCodon codon table]

maxStopCodsDiffOneCod :: [Codon] -> CodingTable -> Codon -> Int
maxStopCodsDiffOneCod stopCodons table stopCod = if not $ null [getBaseDiff stopCod sCod | sCod <- stopCodons]
                                                        then maximum [getBaseDiff stopCod sCod | sCod <- stopCodons]
                                                        else 0

maxMutationsForStopCodons :: GeneticCode -> CodingTable -> Int 
maxMutationsForStopCodons geneCode table = sum [maxStopCodsDiffOneCod allStopCodons table stopCod | stopCod <- allStopCodons] where
    allStopCodons = takeAllStopCodons table geneCode

maxMutations :: CodingTable -> GeneticCode -> Int
maxMutations table geneCode = maxGeneCodeMutations geneCode table + maxMutationsForStopCodons geneCode table

----------------------------------------------------------------------------------------