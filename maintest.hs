import Joueur
import System.IO
import Liste 
import Move


data InfosJeu = InfosJeu String [[String]] [Int] [Int] [[DictionnairePiece]] Int Int Int
-- message jeu 
-- jeu 
-- listeCoord1
-- listeCoord2 
-- dict2D 
-- listeRestantes


jeu = [ ["__", "__", "__", "__"]
                  , ["__", "__", "__", "__"]
                  , ["__", "__", "__", "__"]
                  , ["__", "__", "__", "__"] ]



estUnChar :: Char -> Bool
estUnChar ca = ca == '0'

-- recree la liste dependamment de quelle piece a changer grace a sa taille
{--updateNewListePieceJoueurs :: String -> ListesDispo -> ([[String]],[[String]])
updateNewListePieceJoueurs taille listeModif (ListesDispo liste1 liste2 liste3 liste11 liste22 liste33)
    | (listeModifie !! 0) == 'O' 
        | taille == "B" = ListesDispo listeModifie liste2 liste3 liste11 liste22 liste33
        | taille == "M" = ListesDispo liste1 listeModifie liste3 liste11 liste22 liste33
        | taille == "S" = ListesDispo liste1 liste2 listeModifie liste11 liste22 liste33
        | taille == "T" = ListesDispo liste1 liste2 listeModifie liste11 liste22 liste33
    
    | listeModifie !! 0 == 'X'
        | taille == "B" = [listeModifie,listeMUser,listeSUser,listeTUser]
        | taille == "M" = [listeBUser,listeModifie,listeSUser,listeTUser]
        | taille == "S" = [listeBUser,listeMUser,listeModifie,listeTUser]
        | taille == "T" = [listeBUser,listeMUser,listeSUser,listeModifie]
    
    ListesDispo liste1 liste2 listeModifie liste11 liste22 liste33
--}


-- Fonction pour lire une action depuis l'utilisateur
lectureActionPlayer :: IO String
lectureActionPlayer = do
  putStr "> "  -- Affiche un prompt pour l'utilisateur
  hFlush stdout  -- Vide le tampon de sortie pour s'assurer que le prompt s'affiche immédiatement
  getLine  -- Lit une ligne d'entrée depuis la console



-- Fonction pour afficher un message de bienvenue interactif
messageBienvenue :: IO ()
messageBienvenue = do
  putStrLn "Bienvenue dans le jeu Gobblet !"
  putStrLn "Dans ce jeu, vous pouvez entrer des actions pour interagir."
  putStrLn "Par exemple, vous pouvez entrer 'drop(M, (0,1))' pour jouer une piece à la position (0,1)."
  putStrLn "Entrez 'quitter' pour quitter le jeu."




{--
drop1 :: Player -> String -> Int -> Int -> ListeModifie
drop1 taille x y
  | taille `elem` diffTaillePiece =
    let (pieces, diffPieceJoueur) = case taille of
          "B" -> (listeBUser, listeBUser)
          "M" -> (listeMUser, listeMUser)
          "S" -> (listeSUser, listeSUser)
          "T" -> (listeTUser, listeTUser)
          _   -> ([], ["pas bon"])
        pieceAJouer = retirerTeteListe pieces
        newListePiece = prendResteListe diffPieceJoueur
        jeu1 = modifierCase2D jeu pieceAJouer x y
    in ListeModifie jeu1 newListePiece
  | otherwise = ListeModifie [[]] ["pas bon"]

--}
onboard :: [[String]] -> Int -> Int -> Int -> Int -> [[String]]
onboard jeuDepart y1 x1 y2 x2
  | x1 < 0 || x1 >= length jeuDepart || y1 < 0 || y1 >= length (jeuDepart !! 0) || x2 < 0 || x2 >= length jeuDepart || y2 < 0 || y2 >= length (jeuDepart !! 0) = jeuDepart
  | otherwise =
    let pieceADeplacer = jeuDepart !! x1 !! y1
        jeuTemporaire = modifierCase2D jeuDepart pieceADeplacer x2 y2
        jeuFinal = modifierCase2D jeuTemporaire "__" x1 y1
    in jeuFinal
    


estLeJoueurGagnant :: [Int] -> Int -> Bool 
estLeJoueurGagnant listeComplete i = do
    
    if i < length listeComplete && (i + 1) < length listeComplete
        then do
            let x = listeComplete !! i
                y = listeComplete !! (i + 1)
                listeAlign4 = existeAlign4 [x,y] listeComplete 
        
            if length listeAlign4 == 6
                then True
            else estLeJoueurGagnant listeComplete (i + 2)
    else False
        


peutJouerCase :: [Int] -> Int -> Int -> Int -> Bool
peutJouerCase listePosCasesVide i x y = do
    if i < length listePosCasesVide && (i + 1) < length listePosCasesVide
        then do 
            let x1 = listePosCasesVide !! i 
                y1 = listePosCasesVide !! (i + 1)
            if x1 == x && y1 == y
                then True
            else peutJouerCase listePosCasesVide (i + 2) x y
    else False
                    
    
--verifAlign3PiecesAutreJr listeCoordAdvers 


chercheLeTriplet :: [Int] -> [Int] -> [Int]
chercheLeTriplet listexy listeComplete = do
    let x1 = listexy !! 0
        y1 = listexy !! 1
        x2y2 = retourneSonX2Y2 listexy listeComplete 0
        
    if length x2y2 == 0
        then []
    else do
        let x2 = x2y2 !! 0
            y2 = x2y2 !! 1
            couple = [x1,y1,x2,y2]
            triplet = creerTriplet couple listeComplete 0
            
        if length triplet == 6
            then triplet
        else []

joue :: [[String]] -> String -> Int -> Int -> [[String]]
joue jeu piece x y = modifierCase2D jeu piece x y
    

estUnePieceTailleInf :: String -> String -> Bool
estUnePieceTailleInf taillePieceAdd taillePieceDsJeu = taillePieceAdd < taillePieceDsJeu


renvoieListeJrAdverse :: Int -> [Int] -> [Int] -> [Int]
renvoieListeJrAdverse i listeCoordJr0 listeCoordJr1 = do 
    if i == 0
        then listeCoordJr1
    else listeCoordJr0


estTaPiece :: String -> String -> Bool
estTaPiece (x:xs) (y:ys) = x == y


placeTemporairementPieceEnDessousDsJeu :: [[String]] -> String -> Int -> Int -> [[String]] 
placeTemporairementPieceEnDessousDsJeu jeu pieceEnDessous x y = modifierCase2D jeu pieceEnDessous x y 

{-- 
verifieJaiPasPerdu :: [[String]] -> [Int] -> [Int] -> [[DictionnairePiece]] -> Int -> Int -> Int -> InfosJeu 
verifieJaiPasPerdu jeu listeCoordJr1 listeCoordJr2 liste2DAvecDictionnaire x y i = do
    
    
    --let caseValue = recupereValeurTeteDictio x y liste2DAvecDictionnaire
    
    let cle = recupererCleTeteDictio x y liste2DAvecDictionnaire
    
    case cle of
        Just cle -> do
            let pieceEtListDictio = removeKeyValueAtPosition x y cle liste2DAvecDictionnaire
    
            case pieceEtListDictio of
              Just ((cleEnlevee, valeurEnlevee), updatedList) -> do
                --print cleEnlevee
                --print valeurEnlevee
                
                let caseValueDessous = recupereValeurTeteDictio x y updatedList
                
                case caseValueDessous of
                    Just caseValueDessous -> do
                        let resultat = estTaPiece valeurEnlevee caseValueDessous
                        
                        
                        if resultat
                                then InfosJeu "ma piece tranquillo" jeu listeCoordJr1 listeCoordJr2 updatedList x y i
                            --si cest pas ta piece
                            --verifie si ce jr gagne
                        else do 
                            let newJeu = placeTemporairementPieceEnDessousDsJeu jeu caseValueDessous x y
                            --creer liste coord 
                            let (ListeCoord posPieceJr posPieceOrdi) = creerListeCoordPieces newJeu [] [] 0 0
                            let listeAdversaire = renvoieListeJrAdverse i posPieceOrdi posPieceJr
                            
                            let adversaireAGagne = estLeJoueurGagnant listeAdversaire 0
                            
                            if adversaireAGagne
                                then InfosJeu "vous avez perdu" newJeu listeCoordJr1 listeCoordJr2 updatedList x y i
                            
                            else InfosJeu "" newJeu listeCoordJr1 listeCoordJr2 updatedList x y i

                    Nothing -> InfosJeu "cas3" jeu listeCoordJr1 listeCoordJr2 updatedList x y i
                
              Nothing -> InfosJeu "cas2" jeu listeCoordJr1 listeCoordJr2 liste2DAvecDictionnaire x y i
        Nothing -> InfosJeu "cas1" jeu listeCoordJr1 listeCoordJr2 liste2DAvecDictionnaire x y i
--}



verifieJaiPasPerdu :: InfosJeu -> InfosJeu 
verifieJaiPasPerdu (InfosJeu message jeu listeCoordJr1 listeCoordJr2 liste2DAvecDictionnaire x y i) = do
    
    
    --let caseValue = recupereValeurTeteDictio x y liste2DAvecDictionnaire
    
    let cle = recupererCleTeteDictio x y liste2DAvecDictionnaire
    
    case cle of
        Just cle -> do
            let pieceEtListDictio = removeKeyValueAtPosition x y cle liste2DAvecDictionnaire
    
            case pieceEtListDictio of
              Just ((cleEnlevee, valeurEnlevee), updatedList) -> do
                --print cleEnlevee
                --print valeurEnlevee
                
                let caseValueDessous = recupereValeurTeteDictio x y updatedList
                
                case caseValueDessous of
                    Just caseValueDessous -> do
                        let resultat = estTaPiece valeurEnlevee caseValueDessous
                        
                        
                        if resultat
                                then InfosJeu "ma piece tranquillo" jeu listeCoordJr1 listeCoordJr2 updatedList x y i
                            --si cest pas ta piece
                            --verifie si ce jr gagne
                        else do 
                            let newJeu = placeTemporairementPieceEnDessousDsJeu jeu caseValueDessous x y
                            --creer liste coord 
                            let (ListeCoord posPieceJr posPieceOrdi) = creerListeCoordPieces newJeu [] [] 0 0
                            let listeAdversaire = renvoieListeJrAdverse i posPieceOrdi posPieceJr
                            
                            let adversaireAGagne = estLeJoueurGagnant listeAdversaire 0
                            
                            if adversaireAGagne
                                then InfosJeu "vous avez perdu" newJeu listeCoordJr1 listeCoordJr2 updatedList x y i
                            
                            else InfosJeu "" newJeu listeCoordJr1 listeCoordJr2 updatedList x y i

                    Nothing -> InfosJeu "cas3" jeu listeCoordJr1 listeCoordJr2 updatedList x y i
                
              Nothing -> InfosJeu "cas2" jeu listeCoordJr1 listeCoordJr2 liste2DAvecDictionnaire x y i
        Nothing -> InfosJeu "cas1" jeu listeCoordJr1 listeCoordJr2 liste2DAvecDictionnaire x y i
    




goberUnePiece :: String -> String -> [[String]] -> [[DictionnairePiece]] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> InfosJeu
goberUnePiece taillePieceWantPlay piece jeu liste2DDict listeCoordJr1 listeCoordJr2 x1 y1 x2 y2 i = do 
--gober une piece (mienne ou adverse)
    ---verifie la taille
    --taillePieceAdd est la piece en input
    --taillePieceDsJeu est la piece ds list2dDictio 
    --recupere la taille de la piece a cette pos 
    --let taillePieceDsJeu = recupererCleTeteDictio x y liste2DAvecDictionnaire
    --let estPieceGobable = estUnePieceTailleInf taillePieceAdd taillePieceDsJeu
    --print pieceGobable
    --if estPieceGobable 
    --    then do 
            --ajoute ds liste2DAvecDictionnaire
            --refais tab list coord
            --let newListe2DDictio = addToDictionaryIn2DList x y taillePieceAdd piece liste2DAvecDictionnaire
            --modif case jeu 
            --let jeu2 = modifierCase2D jeu piece x y
            --retourne newListe2DDictio et jeu2 
            --en fait retourne tt les infos du jeu
    --else que faire?
        --ressais infos et essaie de regober
    
    let taillePieceDsJeu = recupererCleTeteDictio x2 y2 liste2DDict
    case taillePieceDsJeu of
        Just taillePieceDsJeu
            | estUnePieceTailleInf taillePieceWantPlay taillePieceDsJeu -> do
                -- Si gobable, effectue les opérations et retourne les informations du jeu
                let newJeu = onboard jeu y1 x1 y2 x2
                    (ListeCoord posPieceJr posPieceOrdi) = creerListeCoordPieces newJeu [] [] 0 0
                    newListe2DDictio = addToDictionaryIn2DList x2 y2 taillePieceWantPlay piece liste2DDict
                InfosJeu "gober" newJeu listeCoordJr1 listeCoordJr2 newListe2DDictio x2 y2 i
            | otherwise -> InfosJeu "non gobable" jeu listeCoordJr1 listeCoordJr2 liste2DDict x2 y2 i
                -- Si non gobable, récupère de nouvelles données et rappelle la fonction
        Nothing -> InfosJeu "piece abscente" jeu listeCoordJr1 listeCoordJr2 liste2DDict x2 y2 i


{- 
userInfo <- resaisirCoup
                let parsedMoves = parseMoves userInfo
                case recupererDropOuOnboardInfo parsedMoves of
                    Just (size, x1, y1, x2, y2) -> do
                        let taille = transformeSizeEnString size
                        goberUnePiece taille piece jeu liste2DAvecDictionnaire listeCoordJr1 listeCoordJr2 x y i
-}

{--
goberUnePiece :: String -> String -> [[String]] -> [[DictionnairePiece]] -> [Int] -> [Int] -> Int -> Int -> Int -> InfosJeu
goberUnePiece taillePieceWantPlay piece jeu liste2DAvecDictionnaire listeCoordJr1 listeCoordJr2 x y i = do
    -- Récupère la taille de la pièce à cette position
    let taillePieceDsJeu = recupererCleTeteDictio x y liste2DAvecDictionnaire
    case taillePieceDsJeu of
        Just taillePieceDsJeu ->
            | estUnePieceTailleInf taillePieceWantPlay taillePieceDsJeu =
                -- Si gobable, effectue les opérations et retourne les informations du jeu
                let newJeu = joue jeu piece x y
                    (ListeCoord posPieceJr posPieceOrdi) = creerListeCoordPieces newJeu [] [] 0 0
                    newListe2DDictio = addToDictionaryIn2DList x y taillePieceWantPlay piece liste2DAvecDictionnaire
                in InfosJeu "gober" newJeu listeCoordJr1 listeCoordJr2 newListe2DDictio x y i
            | otherwise =
                -- Si non gobable, récupère de nouvelles données et rappelle la fonction
                userInfo <- resaisirCoup
                let parsedMoves = parseMoves userInfo
                case recupererDropOuOnboardInfo parsedMoves of
                    Just (size, x1, y1, x2, y2) -> do 
                        let taille = transformeSizeEnString size
                        in goberUnePiece taille piece jeu liste2DAvecDictionnaire listeCoordJr1 listeCoordJr2 x y i
                    Nothing -> InfosJeu "" jeu listeCoordJr1 listeCoordJr2 liste2DAvecDictionnaire x y i
        Nothing -> InfosJeu "" jeu listeCoordJr1 listeCoordJr2 liste2DAvecDictionnaire x y i
            -- Si la taille n'a pas été trouvée, retourne les informations du jeu avec une chaîne vide
   --}         



estUnJeuVide :: [[String]] -> Bool
estUnJeuVide jeu = all (\row -> all (== "__") row) jeu


-- Fonction pour demander à l'utilisateur de saisir une chaîne de caractères
promptString :: String -> IO String
promptString message = do
    putStr message
    hFlush stdout
    getLine

-- Fonction pour saisir les informations de l'utilisateur et les stocker dans un tableau
getUserInfo :: IO [String]
getUserInfo = do
    coup <- promptString "Entrez votre coup : "

    return [coup]


resaisirCoup :: IO [String]
resaisirCoup = do
    putStrLn "Saisissez un nouveau coup :"
    getUserInfo





{--
    
main :: IO ()
main = do
    --putStrLn "Hello gogo"
    
    messageBienvenue
  
    depart <- lectureActionPlayer
    putStrLn $ "Vous avez entré : " ++ depart
    
    --let taillePieceAdd 
    --let piece = 
    
    let listeMovRest1 = [listeBUser,listeMUser,listeSUser,listeTUser]
    let listeMovRest2 = [listeBComp,listeMComp,listeSComp,listeTComp]

    let new1 = formaterListeMov listeMovRest1
    let new2 = formaterListeMov listeMovRest2

    --print jeu
    --putStrLn (new1 ++ " || " ++ new2)
    
    let taille = "B"
    
    let (ListeModifie jeuUpdate liste) = drop1 taille 0 0
    print jeuUpdate
    print liste
    
    let listePieceUser = updateNewListePieceJoueurs taille liste
    
    let new11 = formaterListeMov listePieceUser 
    putStrLn (new11 ++ " || " ++ new2)
    
    ----determiner si un joueur a 4 pieces alignées, si oui fin du jeu, sinon continue
    
    
    putStrLn "Hello World"
    
    
    let test1 = [0,0,1,0,2,0,0,1,1,1,1,2,2,2,0,3,3,3]
    
    let maListe = [1,1,2,3,4,5,2,3]

    let pos1 = existeAlign4 [0,0] test1
    print pos1
    
    let re = estLeJoueurGagnant test1 0
    print re
    
    let (ListeCoord posPieceJr posPieceOrdi) = creerListeCoordPieces jeu [] [] 0 0
    print posPieceJr
    print posPieceOrdi

    let val = estUneCaseVide (jeu !! 0 !! 0)
    print val 
    
    
    ---s'il a droit de jouer ce xy qui est une case vide, fais le drop etc
    
    --si la position de la piece qu'il veut jouer est une case vide , gooo jooue ton drop 
    --Il y a des cases vides?
    
    ---si taille liste /= 0 , il ya des cases vides
    let liste = donneListCasesVide jeu [] 0 0 
    print liste
    
    --est-ce que son mov est valide alors pour une case vide?
    --si true , il peut drop ,sinon il doit recommencer peut-etre en choisissant un autre emplacement(plus tard)
    let rep = peutJouerCase liste 0 0 3
    print rep 
    
    
    ---En premier : verif d'abord que l'autre jr a pas un align de 3 pieces 
    --verifAlign3PiecesAutreJr ListeCoordJrAdvers 
    -- si ListeCoordJrAdvers vide, retourne liste vide 
    --s'il en a 1, retourne la list position de ces 3 pieces et je dois mettre ma piece a la position d'une de ces cases ds le jeu 
    --sinon retourne liste vide 
    
    let triplet = chercheLeTriplet [0,0] [0,0,3,0,0,1,1,1,0,2,0,3]
    print triplet
    
    let rep1 = peutJouerCase triplet 0 0 2
    print rep1
    
    let newJeu = joue jeu "X3" 0 0
    print newJeu
    
    
    printList2DDictio liste2DAvecDictionnaire
    
    -- Exemple d'utilisation : ajouter la clé "B" avec la valeur 123 à la case (1, 2)
    let updatedBigList2D = addToDictionaryIn2DList 1 2 "B" "X3" liste2DAvecDictionnaire
    --printList2D updatedBigList2D
    
    let updatedBigList2D1 = addToDictionaryIn2DList 3 3 "S" "X3" updatedBigList2D
    printList2DDictio updatedBigList2D1
    
    let updatedBigList2D2 = addToDictionaryIn2DList 3 3 "B" "X3" updatedBigList2D1
    printList2DDictio updatedBigList2D2
    
    
    
    -- Exemple d'utilisation : retirer la clé "B" du dictionnaire dans la case (3, 3)
    let pieceEtListDictio = removeKeyValueAtPosition 3 3 "B" updatedBigList2D2
    
    case pieceEtListDictio of
      Just ((cleEnlevee, valeurEnlevee), updatedList) -> do
        putStrLn $ "Clé retirée : " ++ cleEnlevee ++ ", Valeur retirée : " ++ valeurEnlevee
        putStrLn "Liste 2D mise à jour :"
        printList2DDictio updatedList
        let updatedBigList2D3 = addToDictionaryIn2DList 2 3 cleEnlevee valeurEnlevee updatedList
        printList2DDictio updatedBigList2D3
        
        let estDictionnaireVide = tailleDictionnaire 3 3 updatedBigList2D3 0

        -- Afficher le résultat
        if estDictionnaireVide
          then do 
            putStrLn "La taille du dictionnaire dans la case (3, 3) est vide."
            ---remets "__" à la case apropriée
            --let new = modifierCase2D jeu "__" 3 3
            --print new
          else do
            ---recupere la valeur de cette piece a la case 0 , place-là à cette position ds le jeu et retourne le nouveau jeu
            putStrLn "La taille du dictionnaire dans la case (1, 2) n'est pas vide ."
            
            -- Exemple d'utilisation : récupérer la valeur du premier élément du dictionnaire dans la case (1, 2)
            let caseValue = recupereValeurTeteDictio 3 3 updatedBigList2D1

            -- Afficher le résultat
            case caseValue of
              Just value -> do 
                putStrLn $ "Valeur du premier élément dans la case (3, 3) : " ++ value
                --let new = modifierCase2D jeu value 3 3
                --print new
              Nothing -> putStrLn "Case invalide ou dictionnaire vide."

      Nothing ->
        putStrLn "La clé n'a pas été trouvée dans la case spécifiée ou la case est invalide."
        
    
    --faire test après
    let (InfosJeu message jeu listeCoordJr1 listeCoordJr2 updatedList x y i) = verifieJaiPasPerdu jeu [1,1,2,2,3,3,4,4] [4,4] liste2DAvecDictionnaire 2 2 1
    print message
    
    let (InfosJeu message jeu listeCoordJr1 listeCoordJr2 updatedList x y i) = goberUnePiece taillePieceWantPlay piece jeu liste2DAvecDictionnaire listeCoordJr1 listeCoordJr2 x y i
    
    
        
    
    
    
    
    
    --addToDictionaryIn2DList 1 2 "B" "X3" liste2DAvecDictionnaire
    
    ---quand je modifie une case du jr adversaire, je dois modif la pile des pieces en ajoutant la piece avec sa taille
    ---on se rappelle, je dois faire une liste de liste contenant pour chacune un dictionnaire avec pour chaque case, sa cle qui est
    ---la taille et SA VALEUR QUI EST LA PIECE 
    
    
    --if length triplet == 0
        ----joue les cases vides
    --else do
        ------- joue sur une de ces cases triplet
        --let rep1 = peutJouerCase triplet 0 0 3
        -- drop1 sur une de ces cases du triplet
    
    ------------------------------------------------------------------------------------
    
    
    --gober une piece (mienne ou adverse)
    ---verifie la taille
    --taillePieceAdd est la piece en input
    --taillePieceDsJeu est la piece ds list2dDictio 
    --recupere la taille de la piece a cette pos 
    --let taillePieceDsJeu = recupererCleTeteDictio x y liste2DAvecDictionnaire
    --let estPieceGobable = estUnePieceTailleInf taillePieceAdd taillePieceDsJeu
    --print pieceGobable
    --if estPieceGobable 
    --    then do 
            --ajoute ds liste2DAvecDictionnaire
            --refais tab list coord
            --let newListe2DDictio = addToDictionaryIn2DList x y taillePieceAdd piece liste2DAvecDictionnaire
            --modif case jeu 
            --let jeu2 = modifierCase2D jeu piece x y
            --retourne newListe2DDictio et jeu2 
            --en fait retourne tt les infos du jeu
    --else que faire?
    
    
    --soulever ta piece? == retirer piece , il devra plus pouvoir la replacer a cette case avec onboard
    --ya piece en dessous? taille liste2DAvecDictionnaire == 1
    --si oui, utilise la fonction estLeJoueurGagnant listeComplete i
        ---si oui , utilise la fonction gober une piece , s'il peut pas, game over 
        --sinon deplace la piece sur retirer ou tu veux 
    --fonction verifieJaiPasPerdu listeComplete liste2DAvecDictionnaire x y 
    
--}



retourePieceAUnePosition :: [[String]] -> Int -> Int -> String
retourePieceAUnePosition jeu x y = jeu !! x !! y


lancerPartie :: Player -> Int -> ListeModifie -> ListesDispo -> [[DictionnairePiece]] -> IO ()
lancerPartie joueur tour (ListeModifie jeu liste piece) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) liste2D = do
    --let fin = quiAGagne joueur
    --putStrLn fin
    --let joueurSuivant = alternerJoueurs joueur 
    
    
    putStrLn "Informations de l'utilisateur :"
    userInfo <- resaisirCoup

    let parsedMoves = parseMoves userInfo
    --putStrLn "Mouvements analysés :"
    --mapM_ print parsedMoves
    
    let (taille, x1, y1, x2, y2) = case recupererDropOuOnboardInfo parsedMoves of
            Just (size, x1, y1, x2, y2) -> (size, x1, y1, x2, y2)
            Nothing -> (B, -1, -1, -1, -1)  -- Valeurs par défaut si pas de drop/onboard
    
    let tt = transformeSizeEnString taille
    print tt
    print tour
    print x1 
    print y1
    print x2 
    print y2
    
    let tonCoupEstValide = verifierCoup [x1,y1,x2,y2] 
    
    if tonCoupEstValide 
        then do 
            --- joue 
            putStrLn "oui on peut jouer"
            let jeuVide = estUnJeuVide jeu
            --drop(M, (1,1))
            --drop(M, (0,1))
            
            --cas drop
            --tab vide 
            if jeuVide
                then do 
                    if estCasDrop [x2,y2]
                        then do 
                            --printList2DDictio liste2D
                            
                            let (ListeModifie jeuUpdate newListe pieceAJouer) = drop1 joueur jeu tt x1 y1 (ListeModifie jeu liste piece) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                                --(ListesDispo message1 listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) = modifierListeDispo joueur tt newListe (ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                            let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceAJouer liste2D
                            let (ListesDispo message1 liste1 liste2 liste3 liste4 liste5 liste6 liste7 liste8) = modifierListeDispo joueur tt newListe (ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                                --let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceAJouer liste2DAvecDictionnaire
                            putStrLn "biiien tu as affichee"
                            print pieceAJouer
                            printList2DDictio newListe2DDictio
                            --putStrLn "ici taffiches hein"
                            
                            --print jeuUpdate
                            --print listeMU
                            --regler plus tard le cas ou une des listes est vide
                            
                            lancerPartie joueur tour (ListeModifie jeuUpdate newListe pieceAJouer) (ListesDispo "" liste1 liste2 liste3 liste4 liste5 liste6 liste7 liste8) newListe2DDictio
                    
                    --cas onboard     
                    else do 
                        putStrLn "tu peux pas déplacer de pieces, cest un tab vide"
                        lancerPartie joueur tour (ListeModifie jeu liste piece) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) liste2D
            else do 
                putStrLn "ook ca devient interressant"
                
                
                if estCasDrop [x2,y2]
                    then do
                        let pieceACettePosition = retourePieceAUnePosition jeu y1 x1
                        --cas drop case vide
                        if estUneCaseVide pieceACettePosition
                            then do
                                print tt 
                                print listeBU
                                print listeMU
                                
                                
                                let tailleListeCategorie = length liste
                                
                                if tailleListeCategorie == 0
                                    then do 
                                        putStrLn "categorie chosit vide, rejoue avec une autre"
                                        lancerPartie joueur tour (ListeModifie jeu liste piece) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) liste2D

                                else do
                                    let (ListeModifie jeuUpdate newListe pieceAJouer) = drop1 joueur jeu tt x1 y1 (ListeModifie jeu liste pieceACettePosition) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                                    putStrLn "cest cette liste qui correspond a la taille"
                                    print newListe
                                    print jeuUpdate
                                    
                                    print newListe
                                    let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceAJouer liste2D
                                    let (ListesDispo "" liste1 liste2 liste3 liste4 liste5 liste6 liste7 liste8) = modifierListeDispo joueur tt newListe (ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                                    print jeuUpdate
                                    printList2DDictio newListe2DDictio
                                    --print listeMU
                                    --regler plus tard le cas ou une des listes est vide
                                    
                                    lancerPartie joueur tour (ListeModifie jeuUpdate newListe pieceAJouer) (ListesDispo message liste1 liste2 liste3 liste4 liste5 liste6 liste7 liste8) newListe2DDictio
                                        
                                
                                    
                                
                                
                        else do
                        
                            putStrLn "to bad case non vide, faut gober"
                            let tailleListeCategorie = length liste
                            --print liste
                            if tailleListeCategorie == 0
                                then do 
                                    putStrLn "categorie chosit vide, rejoue avec une autre"
                                    lancerPartie joueur tour (ListeModifie jeu liste piece) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) liste2D

                            else do
                            
                                putStrLn "categorie good"
                                let pieceACetteTaille = retournePieceCorrespondante joueur tt (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                                let (InfosJeu messageN jeuUpdate listeCoordJr11 listeCoordJr22 updatedList x y i) = goberUnePiece tt pieceACetteTaille jeu liste2D [] [] x1 y1 x1 y1 0
                                
                                
                                let (ListeModifie jeuNew newListe pieceAJouer) = drop1 joueur jeu tt x1 y1 (ListeModifie jeu liste pieceACetteTaille) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                                let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceACetteTaille liste2D
                                
                                let (ListesDispo message1 liste1 liste2 liste3 liste4 liste5 liste6 liste7 liste8) = modifierListeDispo joueur tt newListe (ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                                printList2DDictio newListe2DDictio
                                print jeuNew
                                
                                putStrLn "liste categorie a la fin:"
                                print newListe 
                                
                                lancerPartie joueur tour (ListeModifie jeuNew newListe pieceAJouer) (ListesDispo message liste1 liste2 liste3 liste4 liste5 liste6 liste7 liste8) newListe2DDictio
                                
                            
                                
                        
                            --putStrLn "to bad case non vide, faut gober"
                            --let pieceACetteTaille = retournePieceCorrespondante joueur tt (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                            --let (InfosJeu messageN jeuUpdate listeCoordJr11 listeCoordJr22 updatedList x y i) = goberUnePiece tt pieceACetteTaille jeu liste2D [] [] x1 y1 x1 y1 0
                            
                            --print messageN
                            --print updatedList
                            --print jeuUpdate
                            
                        
                
                
                else do
                    --onboard((0, 2), (2, 1))
                    putStrLn "cas onboard tab pas vide hehe"
                    
                    let piecePositionDepart = retourePieceAUnePosition jeu y1 x1
                    print piecePositionDepart
                        --cas onboard case de depart
                    if estUneCaseVide piecePositionDepart
                        then do 
                            putStrLn "cas onboard: recommence, tu peux pas déplacer car ya pas de piece à cette case"
                            lancerPartie joueur tour (ListeModifie jeu liste piece) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) liste2D
                    else do 
                        -- no vide
                        --recupere taille de piece a cette position
                        
                        let taillePiece = recupereTailleCorrespondante piecePositionDepart
                        print taillePiece
                        
                        let caseFinale = retourePieceAUnePosition jeu y2 x2
                        
                        if estUneCaseVide caseFinale
                            then do 
                                print "ouiiiii case finale vide"
                                let newJeu = onboard jeu y1 x1 y2 x2
                                let newListe2DDictio = addToDictionaryIn2DList x2 y2 tt piecePositionDepart liste2D
                                --retirerPiece de lancienne case (1,1)
                                --let liste2DUpdate = removeKeyValueAtPosition x1 y1 taillePiece newListe2DDictio
                                
                                let pieceEtListDictio = removeKeyValueAtPosition x1 y1 taillePiece newListe2DDictio
    
                                case pieceEtListDictio of
                                  Just ((cleEnlevee, valeurEnlevee), liste2DUpdate) -> do
                                    print newJeu
                                    printList2DDictio liste2DUpdate
                                  Nothing -> putStrLn "chaud"
                                
                                --let (ListesDispo "" liste1 liste2 liste3 liste4 liste5 liste6 liste7 liste8) = modifierListeDispo joueur tt newListe (ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                                
                        else do 
                            print "piece a case finale a gober quand on deplace"
                            let (InfosJeu messageN jeuUpdate listeCoordJr11 listeCoordJr22 updatedList x y i) = goberUnePiece taillePiece piecePositionDepart jeu liste2D [] [] x1 y1 x2 y2 0
                            --retirer en utilisant onboard ds la fonction gober pour le jeu 
                            --retirer en utilisant removeKey ici 
                            let pieceEtListDictio = removeKeyValueAtPosition x1 y1 taillePiece updatedList
    
                            case pieceEtListDictio of
                              Just ((cleEnlevee, valeurEnlevee), liste2DUpdate) -> do
                                print jeuUpdate
                                printList2DDictio liste2DUpdate
                                lancerPartie joueur tour (ListeModifie jeuUpdate liste piece) (ListesDispo messageN listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) liste2DUpdate
                              Nothing -> putStrLn "chaud"
                            
                            --lancerPartie joueur tour (ListeModifie jeu liste piece) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) liste2DUpdate
                        
                        --let (InfosJeu messageN jeuUpdate listeCoordJr11 listeCoordJr22 updatedList x y i) = goberUnePiece taillePiece piecePositionDepart jeu liste2D [] [] x1 y1 x2 y2 0
                        --goberUnePiece taillePieceWantPlay piece jeu liste2DDict listeCoordJr1 listeCoordJr2 x y i
                        --print messageN
                        --print jeuUpdate
                        --printList2DDictio updatedList
                        
                        --let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceAJouer liste2D
                        --let (ListesDispo "" liste1 liste2 liste3 liste4 liste5 liste6 liste7 liste8) = modifierListeDispo joueur tt newListe (ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                        
                        
                    --cas drop non vide
                    --retourne piece de la taille que je veux
                    {--let pieceACetteTaille = retournePieceCorrespondante joueur tt (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                    let (InfosJeu messageN jeuUpdate listeCoordJr11 listeCoordJr22 updatedList x y i) = goberUnePiece tt pieceACetteTaille jeu liste2D [] [] x1 y1 0
                    
                    print messageN
                    print updatedList
                    print jeuUpdate
                    
                    if messageN == "non gobable"
                        then do
                            putStrLn "piece non gobable, joue un autre coup"
                            lancerPartie joueur tour (ListeModifie jeu liste piece) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) liste2D
                    else putStrLn "gob"
                    --}
                    
                    
                    --cas onboard case vide 
                    
                    
                    
                    
                    
                    --lancerPartie joueur tour (ListeModifie jeu liste piece) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) updatedList
                
                --goberUnePiece :: String -> String -> [[String]] -> [[DictionnairePiece]] -> [Int] -> [Int] -> Int -> Int -> Int -> InfosJeu
                

    else do 
        putStrLn "cest chaud faut recommencer"
        lancerPartie joueur tour (ListeModifie jeu liste piece) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) liste2D
    
    


main :: IO ()
main = do
    {--userInfo <- getUserInfo
    putStrLn "Informations de l'utilisateur :"
    putStrLn $ "Coup : " ++ head userInfo  -- Utilisez "head" pour accéder au premier élément de la liste

    let parsedMoves = parseMoves userInfo
    putStrLn "Mouvements analysés :"
    mapM_ print parsedMoves--}

    -- Récupération de la taille, x et y du premier drop s'il existe
    
    putStrLn "Informations de l'utilisateur :"
    userInfo <- resaisirCoup

    let parsedMoves = parseMoves userInfo
    --putStrLn "Mouvements analysés :"
    --mapM_ print parsedMoves
    
    let (taille, x1, y1, x2, y2) = case recupererDropOuOnboardInfo parsedMoves of
            Just (size, x1, y1, x2, y2) -> (size, x1, y1, x2, y2)
            Nothing -> (B, -1, -1, -1, -1)  -- Valeurs par défaut si pas de drop/onboard
    
    let tt = transformeSizeEnString taille
    print tt
    
    let val = estUnJeuVide jeu
    
    if val 
        then do 
            --let (ListeModifie jeuUpdate liste pieceAJouer) = drop1 Ordi1 jeu tt x1 y1
            --print jeuUpdate 
            --let listeM = updateNewListePieceJoueurs tt liste (ListesDispo liste1 liste2 liste3 liste11 liste22 liste33)
            --print listeM
            --print (listeM !! 0 !! 0 !! 0)
            
            
            
            --let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceAJouer liste2DAvecDictionnaire
            --printList2DDictio newListe2DDictio
            
            putStrLn "bien"
            
            --drop1 :: Player -> [[String]] -> String -> Int -> Int -> (ListeModifie,ListesDispo)
            --drop(B, (1,1))
            --drop(B, (0,0))
            --let (ListeModifie jeuUpdate liste pieceAJouer,ListesDispo mess listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) = drop1 Humain jeu "B" 0 0 (ListeModifie [[]] [] "") (ListesDispo "" listeUserBig listeUserMedium listeUserSmall listeUserTiny listeOrdiBig listeOrdiMedium listeOrdiSmall listeOrdiTiny)
            --print jeuUpdate
            --print listeMU
            
            let p = "choisis une autre catégorie"
            if p == "choisis une autre catégorie"
                then putStrLn "ouiii"
            else putStrLn "noo"
            
            
            --afficheJeuQuandOrdiFinit Ordi1 jeuUpdate
            --alternerJoueurs Player
            
            let go = liste2DAvecDictionnaire
        

            
            
            lancerPartie Humain 0 (ListeModifie jeu [] "") (ListesDispo "" listeUserBig listeUserMedium listeUserSmall listeUserTiny listeOrdiBig listeOrdiMedium listeOrdiSmall listeOrdiTiny) go
            
            
        
            

        else do
            putStrLn "as vide"
            let go = liste2DAvecDictionnaire
            printList2DDictio go
            lancerPartie Humain 0 (ListeModifie jeu [] "") (ListesDispo "" listeUserBig listeUserMedium listeUserSmall listeUserTiny listeOrdiBig listeOrdiMedium listeOrdiSmall listeOrdiTiny) go
    
    
    
