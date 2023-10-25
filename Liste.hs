module Liste where

import qualified Data.Map as Map


data ListeCoord = ListeCoord [Int] [Int]

-- Création d'un dictionnaire
type DictionnairePiece = [(String, String)]

estUneLigneComplete :: Int -> Int -> Int -> Int -> Bool
estUneLigneComplete x1 y1 x2 y2 = estUneLigneCompleteDiag x1 y1 x2 y2 || estUneLigneCompleteDroite x1 y1 x2 y2
    || estUneLigneCompleteBas x1 y1 x2 y2
    
estUneLigneCompleteDiag :: Int -> Int -> Int -> Int -> Bool
estUneLigneCompleteDiag x1 y1 x2 y2 = x2 - x1 == 2 && y2 - y1 == 2

estUneLigneCompleteDroite :: Int -> Int -> Int -> Int -> Bool
estUneLigneCompleteDroite x1 y1 x2 y2 = x2 - x1 == 2 && y2 - y1 == 0

estUneLigneCompleteBas :: Int -> Int -> Int -> Int -> Bool
estUneLigneCompleteBas x1 y1 x2 y2 = x2 - x1 == 0 && y2 - y1 == 2

estUneLigne :: Int -> Int -> Int -> Int -> Bool
estUneLigne x1 y1 x2 y2 =
  (estUneLigneXDroite x1 x2 && estUneLigneYDroite y1 y2)
    || (estUneLigneXBas x1 x2 && estUneLigneYBas y1 y2)
    || (estUneLigneDiagonale x1 x2 y1 y2) || (estUneLigneXDroite x1 x2 && estUneLigneYDroite y1 y2)
    && (estUneLigneXBas x1 x2 && estUneLigneYBas y1 y2)
    && (estUneLigneDiagonale x1 x2 y1 y2) 

supprimerElement :: Eq a => a -> [a] -> [a]
supprimerElement _ [] = []  -- Si la liste est vide, il n'y a rien à supprimer
supprimerElement element (x:xs)
    | element == x = supprimerElement element xs  -- Si l'élément est égal à la tête de la liste, on le saute
    | otherwise = x : supprimerElement element xs 
    

estUneLigneXDroite :: Int -> Int -> Bool
estUneLigneXDroite x1 x2 = x1 + 1 == x2

estUneLigneYDroite :: Int -> Int -> Bool
estUneLigneYDroite y1 y2 = y2 == y1

estUneLigneXBas :: Int -> Int -> Bool
estUneLigneXBas x1 x2 = x1 == x2

estUneLigneYBas :: Int -> Int -> Bool
estUneLigneYBas y1 y2 = y1 + 1 == y2

estUneLigneDiagonale :: Int -> Int -> Int -> Int -> Bool
estUneLigneDiagonale x1 x2 y1 y2 = x1 + 1 == x2 && y1 + 1 == y2

ajouteValeurInListe :: [Int] -> Int -> [Int]
ajouteValeurInListe liste nb = nb : liste


ajouteElementALaFin :: [Int] -> Int -> [Int]
ajouteElementALaFin liste nb = liste ++ [nb]


estUneLigneDe4 :: Int -> Int -> Int -> Int -> Bool
estUneLigneDe4 x1 y1 x4 y4 = estUneLigneDe4Droite x1 y1 x4 y4 || estUneLigneDe4Bas x1 y1 x4 y4 || estUneLigneDe4Diag x1 y1 x4 y4

estUneLigneDe4Droite :: Int -> Int -> Int -> Int -> Bool
estUneLigneDe4Droite x1 y1 x4 y4 = x4 - x1 == 3 && y4 - y1 == 0

estUneLigneDe4Bas :: Int -> Int -> Int -> Int -> Bool
estUneLigneDe4Bas x1 y1 x4 y4 = x4 - x1 == 0 && y4 - y1 == 3

estUneLigneDe4Diag :: Int -> Int -> Int -> Int -> Bool
estUneLigneDe4Diag x1 y1 x4 y4 = x4 - x1 == 3 && y4 - y1 == 3



supprimerElementALaPosition :: Int -> [a] -> [a]
supprimerElementALaPosition _ [] = []
supprimerElementALaPosition 0 (_:xs) = xs
supprimerElementALaPosition n (x:xs)
    | n < 0 = x:xs  -- Si la position est négative, la liste reste inchangée
    | otherwise = x : supprimerElementALaPosition (n - 1) xs


existeAlign4 :: [Int] -> [Int] -> [Int]
existeAlign4 x1y1 listeComplete  = do

    let listeTrouve = chercherAlignement4Pieces x1y1 listeComplete [] [] 0
    
    if length listeTrouve == 6
        then listeTrouve
    else if length listeTrouve == 0
        then []
    else 
        let newListeComplete = enleverElementsInutiles x1y1 listeComplete 0 0
        in existeAlign4 x1y1 newListeComplete 



--renvoie la nouvelle liste complete 
enleverElementsInutiles :: [Int] -> [Int] -> Int -> Int -> [Int]
enleverElementsInutiles listePosXY listeComplete i decalage = do

    if i < length listePosXY
        then do 
            let position = (listePosXY !! i) - decalage
                element = (listeComplete !! position)
                newListeComplete = supprimerElementALaPosition position listeComplete
            enleverElementsInutiles listePosXY newListeComplete (i + 1) (decalage + 1)
    else listeComplete
    
    

creerListeCoordPieces :: [[String]] -> [Int] -> [Int] -> Int -> Int -> ListeCoord
creerListeCoordPieces jeu pieceJr pieceCom i j = do
    if i < length jeu
        then do
            if j < length jeu
                then do
                    let casee = jeu !! i !! j
            
                    if casee == "X3" || casee == "X2" || casee == "X1" || casee == "X0"
                        then do
                            let newPieceJr = ajouteElementALaFin pieceJr j
                                newUpdate = ajouteElementALaFin newPieceJr i
                            creerListeCoordPieces jeu newUpdate pieceCom i (j + 1)
                    else if casee == "O3" || casee == "O2" || casee == "O1" || casee == "O0"
                        then do 
                            let newPieceCom = ajouteElementALaFin pieceCom j
                                newUpdate = ajouteElementALaFin newPieceCom i
                            creerListeCoordPieces jeu pieceJr newUpdate i (j + 1)
                    else creerListeCoordPieces jeu pieceJr pieceCom i (j + 1)
            else creerListeCoordPieces jeu pieceJr pieceCom (i + 1) 0
    else ListeCoord pieceJr pieceCom
                  
          
                    

--retourne position xy des 4 pieces
chercherAlignement4Pieces :: [Int] -> [Int] -> [Int] -> [Int] -> Int -> [Int]
chercherAlignement4Pieces listexy listeComplete listeAlign4 listePosXY i = do
    let x1 = listexy !! 0
        y1 = listexy !! 1
        
    if i < length listeComplete && (i + 1) < length listeComplete
        then do
            let x2 = listeComplete !! i 
                y2 = listeComplete !! (i + 1)
                
            if length listeAlign4 == 0
                then do
                    if estUneLigne x1 y1 x2 y2
                        then do
                            let newListeAlign4 = ajouteElementALaFin listeAlign4 x2
                                update = ajouteElementALaFin newListeAlign4 y2
                                posXY = ajouteElementALaFin listePosXY i
                                posXYUpdate = ajouteElementALaFin posXY (i + 1)
                            chercherAlignement4Pieces listexy listeComplete update posXYUpdate(i + 2)
                    else chercherAlignement4Pieces listexy listeComplete listeAlign4 listePosXY (i + 2)
            else if length listeAlign4 == 2
                then do
                    let x = listeAlign4 !! 0
                        y = listeAlign4 !! 1
                    if estUneLigne x y x2 y2 && estUneLigneComplete x1 y1 x2 y2
                        then do
                            let newListeAlign4 = ajouteElementALaFin listeAlign4 x2
                                update = ajouteElementALaFin newListeAlign4 y2
                                posXY = ajouteElementALaFin listePosXY i
                                posXYUpdate = ajouteElementALaFin posXY (i + 1)
                            chercherAlignement4Pieces listexy listeComplete update posXYUpdate (i + 2)
                    else chercherAlignement4Pieces listexy listeComplete listeAlign4 listePosXY (i + 2)
            else if length listeAlign4 == 4
                then do
                    let x = listeAlign4 !! 0
                        y = listeAlign4 !! 1  
                        x3 = listeAlign4 !! 2
                        y3 = listeAlign4 !! 3
                    if estUneLigne x3 y3 x2 y2 && estUneLigneComplete x y x2 y2 && estUneLigneDe4 x1 y1 x2 y2
                        then do
                            let newListeAlign4 = ajouteElementALaFin listeAlign4 x2
                                update = ajouteElementALaFin newListeAlign4 y2
                                posXY = ajouteElementALaFin listePosXY i
                                posXYUpdate = ajouteElementALaFin posXY (i + 1)
                            chercherAlignement4Pieces listexy listeComplete update posXYUpdate (i + 2)
                    else chercherAlignement4Pieces listexy listeComplete listeAlign4 listePosXY (i + 2)
            else listePosXY
    else listePosXY



donneListCasesVide :: [[String]] -> [Int] -> Int -> Int -> [Int]
donneListCasesVide jeu listePosCasesVides i j = do 
    if i < length jeu 
        then do
            if j < length jeu
                then do 
                    let casee = (jeu !! i !! j)
                    if estUneCaseVide casee
                        then do 
                            let list1 = ajouteElementALaFin listePosCasesVides j
                                list2 = ajouteElementALaFin list1 i
                            donneListCasesVide jeu list2 i (j + 1)
                    else donneListCasesVide jeu listePosCasesVides i (j + 1)
            else donneListCasesVide jeu listePosCasesVides (i + 1) 0
    else listePosCasesVides


estUneCaseVide :: String -> Bool
estUneCaseVide casee = casee == "__"



creerTriplet :: [Int] -> [Int] -> Int -> [Int]
creerTriplet couple listeComplete i = do
    let x1 = couple !! 0
        y1 = couple !! 1
        x2 = couple !! 2
        y2 = couple !! 3
    
    if i < length listeComplete && (i + 1) < length listeComplete
        then do
            let x3 = listeComplete !! i
                y3 = listeComplete !! (i + 1)
            
            if estUneLigne x2 y2 x3 y3 && estUneLigneComplete x1 y1 x3 y3
                then [x1,y1,x2,y2,x3,y3]
            else creerTriplet couple listeComplete (i + 2)
    else couple



retourneSonX2Y2 :: [Int] -> [Int] -> Int -> [Int]
retourneSonX2Y2 x1y1 listeNouvelle i = do
    if length listeNouvelle == 2
        then []
    else if i < length listeNouvelle && (i + 1) < length listeNouvelle
        then do
            let x2 = listeNouvelle !! i
                y2 = listeNouvelle !! (i + 1)
                x1 = x1y1 !! 0
                y1 = x1y1 !! 1
            if estUneLigne x1 y1 x2 y2
                then [x2,y2]
            else retourneSonX2Y2 x1y1 listeNouvelle (i + 2)
    else []


compteAvecListeComplete :: [Int] -> [Int] -> [Int] -> [Int]
compteAvecListeComplete [] _ listeCompteur = listeCompteur
compteAvecListeComplete _ [] listeCompteur = listeCompteur
compteAvecListeComplete (x1:y1:rest) listeComplete listeCompteur =
    let new = comptePourChacunFinal [x1, y1] listeComplete listeCompteur
    in compteAvecListeComplete rest listeComplete new


comptePourChacunFinal :: [Int] -> [Int] -> [Int] -> [Int]
comptePourChacunFinal listexy listeComplete listeCompteur = do
    let x1 = listexy !! 0
        y1 = listexy !! 1
        x2y2 = retourneSonX2Y2 listexy listeComplete 0
        
    if length x2y2 == 0
        then ajouteValeurInListe listeCompteur 0
    else do
        let x2 = x2y2 !! 0
            y2 = x2y2 !! 1
            couple = [x1,y1,x2,y2]
            triplet = creerTriplet couple listeComplete 0
            
        if length triplet == 6
            then ajouteValeurInListe listeCompteur 1
        else ajouteValeurInListe listeCompteur 0 


-- Exemple d'une liste 2D de 4x4 cases avec des dictionnaires vides
liste2DAvecDictionnaire :: [[DictionnairePiece]]
liste2DAvecDictionnaire = createListVide2D 4 4



createListVide2D :: Int -> Int -> [[DictionnairePiece]]
createListVide2D ligne col =
  replicate ligne (replicate col [])


--- Fonction pour ajouter une clé et une valeur au début d'un dictionnaire dans une case 2D
addToDictionaryIn2DList :: Int -> Int -> String -> String -> [[DictionnairePiece]] -> [[DictionnairePiece]]
addToDictionaryIn2DList row col key value myList =
    let prependKeyValue dictList = (key, value) : dictList
        updatedRow = take row myList ++ [take col (myList !! row) ++ [prependKeyValue (myList !! row !! col)] ++ drop (col + 1) (myList !! row)] ++ drop (row + 1) myList
    in updatedRow
  
  
 
printList2DDictio :: [[DictionnairePiece]] -> IO ()
printList2DDictio myList =
    mapM_ printRowWithLabel (zip [0..] myList)
    where
        printRowWithLabel (rowIdx, row) = do
          putStrLn $ "Case " ++ show rowIdx ++ ":"
          mapM_ printDictionaryWithLabel row
          putStrLn ""  -- Ajoute une ligne vide entre les lignes
        
        printDictionaryWithLabel dictList = do
          putStrLn "  Dictionnaire :"
          mapM_ printKeyValue dictList
        
        printKeyValue (key, value) = putStrLn $ "    " ++ key ++ ": " ++ value

-- Fonction pour déterminer la taille d'un dictionnaire dans une case 2D
dictionarySizeIn2DList :: Int -> Int -> [[DictionnairePiece]] -> Maybe Int
dictionarySizeIn2DList row col myList =
  if row >= 0 && col >= 0 && row < length myList && col < length (myList !! row)
    then Just (length (myList !! row !! col))
    else Nothing


-- Comparer la taille du dictionnaire dans une case spécifique à une valeur donnée
tailleDictionnaire :: Int -> Int -> [[DictionnairePiece]] -> Int -> Bool
tailleDictionnaire row col myList taille1 =
  case dictionarySizeIn2DList row col myList of
    Just taille -> taille == taille1
    Nothing   -> False
    



-- Fonction pour retirer une clé-valeur à une certaine position d'un dictionnaire
-- et renvoyer cette clé-valeur ainsi que le dictionnaire modifié
removeKeyValueAtPosition :: Int -> Int -> String -> [[DictionnairePiece]] -> Maybe ((String, String), [[DictionnairePiece]])
removeKeyValueAtPosition row col key myList
  | row < 0 || col < 0 || row >= length myList || col >= length (myList !! row) = Nothing
  | otherwise =
    let currentDict = myList !! row !! col
        (removedPair, updatedDict) = removePairByKey key currentDict
        updatedRow = take row myList ++ [take col (myList !! row) ++ [updatedDict] ++ drop (col + 1) (myList !! row)] ++ drop (row + 1) myList
    in Just (removedPair, updatedRow)
    

-- Fonction pour retirer une paire (clé, valeur) d'une liste de paires par clé
removePairByKey :: String -> DictionnairePiece -> ((String, String), DictionnairePiece)
removePairByKey key dictList =
    case span (\(k, _) -> k /= key) dictList of
    (before, (_, value):after) -> ((key, value), before ++ after)
    _ -> ((key, ""), dictList)  -- Clé non trouvée


-- Fonction pour récupérer la valeur du premier élément du dictionnaire dans une case 2D
recupereValeurTeteDictio :: Int -> Int -> [[DictionnairePiece]] -> Maybe String
recupereValeurTeteDictio row col myList
  | row < 0 || col < 0 || row >= length myList || col >= length (myList !! row) = Nothing
  | otherwise =
    case myList !! row !! col of
      (key, value):_ -> Just value
      _              -> Nothing
      

-- Fonction pour récupérer la clé du premier élément du dictionnaire dans une case 2D
recupererCleTeteDictio :: Int -> Int -> [[DictionnairePiece]] -> Maybe String
recupererCleTeteDictio row col myList
  | row < 0 || col < 0 || row >= length myList || col >= length (myList !! row) = Nothing
  | otherwise =
    case myList !! row !! col of
      (key, _):_ -> Just key
      _          -> Nothing
      


