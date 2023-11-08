{- |
Module      :  Main
Description :  Rassemble les fonctionnalités pour lancer le jeu de Gobelets
                Le programme prend les entrées des utilisateurs et effectue un mouvement
                quand celui-ci est valide, sinon on redemande a l'utilisateur de resaisir 
                son mouvement
Copyright   :  Oswald Essongué
License     :  <license>

Maintainer  :  cd891906@ens.uqam.ca
Stability   :  stable 
Portability :  portable 

-}


import System.IO (withFile, IOMode(AppendMode), hPutStrLn)
import Joueur
import System.IO
import Liste 
import Move
import Debug.Trace

data InfosJeu = InfosJeu String [[String]] [Int] [Int] [[DictionnairePiece]] Int Int Int



data InfosMeilleurCoup = InfosMeilleurCoup Int Int Int 


jeu = [ ["__", "__", "__", "__"]
                  , ["__", "__", "__", "__"]
                  , ["__", "__", "__", "__"]
                  , ["__", "__", "__", "__"] ]


-- | Fonction pour lire une action de l'utilisateur
lectureActionPlayer :: IO String
lectureActionPlayer = do
  putStr "> "  
  hFlush stdout  
  getLine  


-- | Fonction pour afficher un message de bienvenue interactif
messageBienvenue :: IO ()
messageBienvenue = do
  putStrLn "Bienvenue dans le jeu Gobblet !"
  putStrLn "Dans ce jeu, vous pouvez entrer des actions pour interagir."
  putStrLn "Par exemple, vous pouvez entrer 'drop(M, (0,1))' pour jouer une piece à la position (0,1)."
  putStrLn " Plus tard vous pourre aussi deplacer cette piece comme ceci par exemple 'onboard((0, 1), (2, 1))'"
  putStrLn "Entrez 'quitter' pour quitter le jeu."



-- | Cette fonction déplace une piece d'une case de depart y1 x1 a y2 x2
onboard :: [[String]] -> Int -> Int -> Int -> Int -> [[String]]
onboard jeuDepart y1 x1 y2 x2
  | x1 < 0 || x1 >= length jeuDepart || y1 < 0 || y1 >= length (jeuDepart !! 0) || x2 < 0 || x2 >= length jeuDepart || y2 < 0 || y2 >= length (jeuDepart !! 0) = jeuDepart
  | otherwise =
    let pieceADeplacer = jeuDepart !! x1 !! y1
        jeuTemporaire = modifierCase2D jeuDepart pieceADeplacer x2 y2
        jeuFinal = modifierCase2D jeuTemporaire "__" x1 y1
    in jeuFinal
    

-- | Cette fonction détermine si un joueur est gagnant a partir de sa liste de position des piceces
-- | qu'il a joué
estLeJoueurGagnant :: [Int] -> Int -> Bool 
estLeJoueurGagnant [] _ = False
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
        

-- | Cette fonction cherche a trouver un alignement de 3
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
    

-- | Cette fonction determine si une la taille d'une piece que le joueur veut jouer
-- | est inférieure a celle qui est dans le jeu 
estUnePieceTailleInf :: String -> String -> Bool
estUnePieceTailleInf taillePieceAdd taillePieceDsJeu = taillePieceAdd < taillePieceDsJeu


-- | Cette fonction permet de gober une piece et d'ajouter la nouvelle piece 
-- | dans un DictionnairePiece
goberUnePiece :: String -> String -> [[String]] -> [[DictionnairePiece]] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> InfosJeu
goberUnePiece taillePieceWantPlay piece jeu liste2DDict listeCoordJr1 listeCoordJr2 x1 y1 x2 y2 i = do 
    
    let taillePieceDsJeu = recupererCleTeteDictio x2 y2 liste2DDict
    case taillePieceDsJeu of
        Just taillePieceDsJeu
            | estUnePieceTailleInf taillePieceWantPlay taillePieceDsJeu -> do
                -- Si gobable, effectue les opérations et retourne les informations du jeu
                let newJeu = onboard jeu x1 y1 x2 y2
                    (ListeCoord posPieceJr posPieceOrdi) = creerListeCoordPieces newJeu [] [] 0 0
                    newListe2DDictio = addToDictionaryIn2DList x2 y2 taillePieceWantPlay piece liste2DDict
                InfosJeu "gober" newJeu listeCoordJr1 listeCoordJr2 newListe2DDictio x2 y2 i
            | otherwise -> InfosJeu "non gobable" jeu listeCoordJr1 listeCoordJr2 liste2DDict x2 y2 i
                -- Si non gobable, récupère de nouvelles données et rappelle la fonction
        Nothing -> InfosJeu "piece abscente" jeu listeCoordJr1 listeCoordJr2 liste2DDict x2 y2 i


-- | Cette fonction détermine si un jeu est vide 
estUnJeuVide :: [[String]] -> Bool
estUnJeuVide jeu = all (\row -> all (== "__") row) jeu


-- Fonction pour demander à l'utilisateur de saisir une chaîne de caractères
promptString :: String -> IO String
promptString message = do
    putStr message
    hFlush stdout
    getLine

-- | Fonction pour saisir les informations de l'utilisateur et les stocker dans un tableau
getUserInfo :: IO [String]
getUserInfo = do
    coup <- promptString "Entrez votre coup : "

    return [coup]

-- | Cette fonction permet de resaisir un coup 
resaisirCoup :: IO [String]
resaisirCoup = do
    putStrLn "Saisissez un nouveau coup :"
    getUserInfo



-- | Cette fonction va vérifier si le joueur adversaire va jouer sur une case du joueur actuel 
vaJouerSurUneCaseAdverse :: [Int] -> [Int] -> Bool
vaJouerSurUneCaseAdverse _ [] = False
vaJouerSurUneCaseAdverse listeXYJoueurActuel (x:y:xs) 
    | (listeXYJoueurActuel !! 0) == x && (listeXYJoueurActuel !! 1) == y = True 
    | otherwise = vaJouerSurUneCaseAdverse listeXYJoueurActuel xs



faireSonMeilleurCoup :: Player -> [[String]] -> Int -> Int -> [String] -> InfosMeilleurCoup -> InfosMeilleurCoup
faireSonMeilleurCoup joueur jeu1 j i deckOrd1 (InfosMeilleurCoup x y scoreDepart)
    | i < length jeu1 = -- Vérification de la limite en i
        if j < length jeu1
            then do 
                let (InfosMeilleurCoup x y score) = faireSonMeilleurCoupPourUnPoint joueur jeu1 j i deckOrd1 (InfosMeilleurCoup x y scoreDepart)
                faireSonMeilleurCoup joueur jeu1 (j + 1) i deckOrd1 (InfosMeilleurCoup x y score) 
            else
                faireSonMeilleurCoup joueur jeu1 0 (i + 1) deckOrd1 (InfosMeilleurCoup x y scoreDepart)
    | otherwise = InfosMeilleurCoup x y scoreDepart

faireSonMeilleurCoupPourUnPoint :: Player -> [[String]] -> Int -> Int -> [String] -> InfosMeilleurCoup -> InfosMeilleurCoup
faireSonMeilleurCoupPourUnPoint joueur jeu1 j i deckOrd1 (InfosMeilleurCoup x y scoreDepart)
    | joueur == Ordi1 =
        let casee = retourePieceAUnePosition jeu1 j i -- Utilisation de i et j dans l'ordre (i j)
        in
        if estUneCaseVide casee
            then
                let jeu2 = modifierCase2D jeu1 (head deckOrd1) i j -- Utilisation de i et j dans l'ordre (i j)
                    (ListeCoord liste1 listePosPiecOrd1) = creerListeCoordPieces jeu2 [] [] 0 0
                    nbALign3 = sum (compteAvecListeComplete listePosPiecOrd1 listePosPiecOrd1 [])
                    nbALign2 = calculerAlignement2 listePosPiecOrd1
                    score = nbALign2 + nbALign3
                in
                if score > scoreDepart
                    then InfosMeilleurCoup j i score
                    else InfosMeilleurCoup j i scoreDepart
        else
            let taillePieceWantPlay = recupereTailleCorrespondante (head deckOrd1)
                taillePieceDsJeu = recupereTailleCorrespondante casee
            in
            if estUnePieceTailleInf taillePieceWantPlay taillePieceDsJeu
                then
                    let jeu2 = modifierCase2D jeu1 (head deckOrd1) i j -- Utilisation de i et j dans l'ordre (i j)
                        (ListeCoord liste1 listePosPiecOrd1) = creerListeCoordPieces jeu2 [] [] 0 0
                        nbALign3 = sum (compteAvecListeComplete listePosPiecOrd1 listePosPiecOrd1 [])
                        nbALign2 = calculerAlignement2 listePosPiecOrd1
                        score = nbALign2 + nbALign3
                    in
                    if score > scoreDepart
                        then InfosMeilleurCoup j i score
                        else InfosMeilleurCoup j i scoreDepart
            else InfosMeilleurCoup j i scoreDepart
    | otherwise = InfosMeilleurCoup (-1) (-1) (-1)



-- | Cette fonction va vérifier s'il existe un alignement de 3 
-- | dans la liste des positions des pieces du joueur 
existence1Align3 :: [Int] -> [Int] -> String
existence1Align3 [] _ = "non"
existence1Align3 listePosPieceJr listeNbALign3 = do
    let listeNbALign3' = compteAvecListeComplete listePosPieceJr listePosPieceJr listeNbALign3
    let existe1ALign3 = any (\x -> x == 1) listeNbALign3'
    
    if existe1ALign3 
        then "oui gros ya 1 align de 3"
        else "non"


-- Cette fonction détermine qui joue actuellement
determinerJoueurActuel :: Player -> String
determinerJoueurActuel Humain = "Humain"
determinerJoueurActuel Ordi1 = "Ordi1"


-- | Cette fonction détermine qui est le joueur adverse 
voiciLeJoueurAdverse :: Player -> Player
voiciLeJoueurAdverse Humain = Ordi1
voiciLeJoueurAdverse Ordi1 = Humain


-- | Cette fonction va retourner la liste des positions des pieces
-- | du joueur adverse
retourneListePieceAdversaire :: Player -> [Int] -> [Int] -> [Int]
retourneListePieceAdversaire joueurAdverse listePosPiecHumain listePosPiecOrd1
    | joueurAdverse == Humain = listePosPiecHumain
    | joueurAdverse == Ordi1 = listePosPiecOrd1
    | otherwise = []


-- | Cette fonction va déterminer si un joueur peut va faire son coup sur une case adverse
-- | dans le cas ou l'adversaire a un alignement de 3 pieces 
peutFaireSonCoup :: String -> [Int] -> [Int] -> [Int] -> String
peutFaireSonCoup joueurActuel xyDuDrop listePosPiecHumain listePosPiecOrd1 = do
    if joueurActuel == "Humain"
        then do 
            if vaJouerSurUneCaseAdverse xyDuDrop listePosPiecOrd1
                then "oui il va jouer sur une case de adversaire"
            else "noon"
    else do 
        if vaJouerSurUneCaseAdverse xyDuDrop listePosPiecHumain
            then "oui il va jouer sur une case de adversaire"
        else "noon"


-- | Cette fonction retorune la piece a une certaine position 
retourePieceAUnePosition :: [[String]] -> Int -> Int -> String
retourePieceAUnePosition jeu x y = jeu !! x !! y


{--
applyMoves :: [Move] -> IO()
applyMoves (x:xs) = do 
    print x
    applyMoves xs
    --let jeuFinal = lancerPartiePourUnMove joueur moves (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
    


lancerPartiePourUnMove :: Player -> [Move] -> ListeModifie -> [[DictionnairePiece]] -> [[String]]
lancerPartiePourUnMove joueur moves (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D = do 

    let joueurSuivant = alternerJoueurs joueur 
    let playerNow = quiJoueNow joueur

    let (taille, x1, y1, x2, y2) = case recupererDropOuOnboardInfo moves of
            Just (size, x1, y1, x2, y2) -> (size, x1, y1, x2, y2)
            Nothing -> (B, -1, -1, -1, -1)  -- Valeurs par défaut si pas de drop/onboard
    
    let tt = transformeSizeEnString taille

    let tonCoupEstValide = verifierCoup [x1,y1,x2,y2] 
    
    if tonCoupEstValide 
        then do 
            --- joue 
            
            let jeuVide = estUnJeuVide jeu

            if jeuVide
                then do 
                    if estCasDrop [x2,y2] 
                        then do 
                            --verifie ordre des tailles avant de jouer ds le deck 
                            
                            let bonOrdre = piecePrisDansBonOrdre joueur tt (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1))

                            if bonOrdre == "oui"
                                then do 
                                
                                    let (ListeModifie jeuUpdate (ListesDispo (Deck Humain newdecHum) (Deck Ordi1 newDeckOrd1)) pieceAJouer2) = drop1 joueur jeu y1 x1 (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1)
                                    let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceAJouer2 liste2D
                                    
                                    lancerPartiePourUnMove joueurSuivant moves (ListeModifie jeuUpdate (ListesDispo (Deck Humain newdecHum) (Deck Ordi1 newDeckOrd1)) pieceAJouer2) newListe2DDictio
                                    
                            else lancerPartiePourUnMove joueur moves (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D

                    --cas onboard     
                    else lancerPartiePourUnMove joueur moves (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
            else do 
                --putStrLn "ook ca devient interressant, tab non vide"
                
                
                if estCasDrop [x2,y2]
                    then do
                    
                        --verifie ordre des tailles avant de jouer ds le deck
                        let bonOrdre = piecePrisDansBonOrdre joueur tt (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1))
                        
                        if bonOrdre == "oui"
                            then do
                                
                                ----determiner pieces de adversaire
                                ----determiner dabord qui est joueur actuel pour deduire qui est adversaire
                                
                                let (ListeCoord listePosPiecHumain listePosPiecOrd1) = creerListeCoordPieces jeu [] [] 0 0
                                --string humain ou joueur
                                let joueurActuel = determinerJoueurActuel joueur 
                                
                                let adversaire = voiciLeJoueurAdverse joueur
                                
                                 
                                --let (ListeCoord h c) = creerListPieces2JoueursAvecDictio jeu 0 0 [] [] liste2D
                                
                                --creerListPieces2JoueursAvecDictioPrint jeu 0 0 [] [] liste2D

                                let listePosPieceAdversaire = retourneListePieceAdversaire adversaire listePosPiecHumain listePosPiecOrd1
                                
                                let joueurAdversePossedAlign3 = existence1Align3 listePosPieceAdversaire []
                               
                                if joueurAdversePossedAlign3 == "non"
                                    then do
                                        
        -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                        let pieceACettePosition = retourePieceAUnePosition jeu y1 x1
                                        --cas drop case vide
                                        if estUneCaseVide pieceACettePosition
                                            then do
                                                --let pieceACetteTaille = trace ("aaaaaa" ++ tt ++ message) $ retournePieceCorrespondante joueur tt (ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                                                
                                                let (ListeModifie jeuUpdate (ListesDispo (Deck Humain newdecHum) (Deck Ordi1 newDeckOrd1)) pieceAJouer2) = drop1 joueur jeu y1 x1 (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1)
                                                let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceAJouer2 liste2D
                
                                                
                                                
                                                
                                                lancerPartiePourUnMove joueurSuivant moves (ListeModifie jeuUpdate (ListesDispo (Deck Humain newdecHum) (Deck Ordi1 newDeckOrd1)) pieceAJouer2) newListe2DDictio
                                                    
                                        else do
                                        
                                            
                                            --est une piece gobable?
                                            
                                            --recuere piece a cette taille tt 
                                            --je fais juste recuperer la piec de tt qui sera pieceACetteTaille
                                            let pieceACetteTaille = retournePieceCorrespondante joueur tt
                                            let (InfosJeu messageN jeuUpdate listeCoordJr11 listeCoordJr22 updatedList x y i) = goberUnePiece tt pieceACetteTaille jeu liste2D [] [] x1 y1 x1 y1 0
                                    
                                            if messageN == "non gobable"
                                                then lancerPartiePourUnMove joueur moves (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
                                                
                                            else do 
                                                let (ListeModifie jeuNew (ListesDispo (Deck Humain deckHum1) (Deck Ordi1 deckOrd1)) pieceAJouer2) = drop1 joueur jeuUpdate y1 x1 (ListeModifie jeuUpdate (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1)
                                                let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceACetteTaille liste2D 
                                                
                                                
                                                lancerPartiePourUnMove joueurSuivant moves (ListeModifie jeuNew (ListesDispo (Deck Humain deckHum1) (Deck Ordi1 deckOrd1)) pieceAJouer2) newListe2DDictio
        -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                        
                                    
                                else do 
                                    
                                    --compare le xy de drop avec listePiecesJrAdvers
                                    --si vaJouerSurUneCaseAdverse listePiecesJrAdvers est True , joue normalement
                                    --sinon redemande en disant joueur actuel doit jouer sur case jr adverse
                                    
                                    
                                    
                                    --est-ce que joueurActuel va jouer sur une case du joueur adverse
                                    
                                    --remplacer ce if else par ma fonction peutFaireSonCoup joueur
                                    let reponse = peutFaireSonCoup joueurActuel [x1,y1] listePosPiecHumain listePosPiecOrd1
                                    --------------------------------------------------------------------
                                    
                                    if reponse == "noon"
                                        then lancerPartiePourUnMove joueur moves (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
                                    else do 
                                        ---je peux jouer, alors je vais devoir gober la piece du joueur adverse 
                                        ---ici je sais deja que la case de cette piece est occupée donc as besoin de vérifier si la case est vide ou non
                                        let pieceACetteTaille = retournePieceCorrespondante joueur tt
                                        let (InfosJeu messageN jeuUpdate listeCoordJr11 listeCoordJr22 updatedList x y i) = goberUnePiece tt pieceACetteTaille jeu liste2D [] [] x1 y1 x1 y1 0
                                
                                        if messageN == "non gobable"
                                            then lancerPartiePourUnMove joueur moves (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
                                            
                                        else do 
                                            let (ListeModifie jeuNew (ListesDispo (Deck Humain deckHum1) (Deck Ordi1 deckOrd1)) pieceAJouer2) = drop1 joueur jeuUpdate y1 x1 (ListeModifie jeuUpdate (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1)
                                            let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceACetteTaille liste2D 
                                            
                                            
                                            lancerPartiePourUnMove joueurSuivant moves (ListeModifie jeuNew (ListesDispo (Deck Humain deckHum1) (Deck Ordi1 deckOrd1)) pieceAJouer2) newListe2DDictio

                        else lancerPartiePourUnMove joueur moves (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D

                else do
                    --onboard((0, 2), (2, 1))
                    
                    
                    let piecePositionDepart = retourePieceAUnePosition jeu y1 x1
                        --cas onboard case de depart
                    if estUneCaseVide piecePositionDepart
                        then lancerPartiePourUnMove joueur moves (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D 
                    else do 
                        -- no vide
                        --recupere taille de piece a cette position
                        
                        let taillePiece = recupereTailleCorrespondante piecePositionDepart 

                        let caseFinale = retourePieceAUnePosition jeu y2 x2
                        
                        if estUneCaseVide caseFinale
                            then do 
                               
                                let newJeu = onboard jeu x1 y1 x2 y2
                                let newListe2DDictio = addToDictionaryIn2DList x2 y2 tt piecePositionDepart liste2D
                                --retirerPiece de lancienne case (1,1)
                                --let liste2DUpdate = removeKeyValueAtPosition x1 y1 taillePiece newListe2DDictio
                                
                                let pieceEtListDictio = removeKeyValueAtPosition x1 y1 taillePiece newListe2DDictio
    
                                case pieceEtListDictio of
                                  Just ((cleEnlevee, valeurEnlevee), liste2DUpdate) -> 
                                    lancerPartiePourUnMove joueurSuivant moves (ListeModifie newJeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2DUpdate
                                    
                                  Nothing -> lancerPartiePourUnMove joueurSuivant moves (ListeModifie newJeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) newListe2DDictio
                                
                        else do 
                            let (InfosJeu messageN jeuUpdate listeCoordJr11 listeCoordJr22 updatedList x y i) = goberUnePiece taillePiece piecePositionDepart jeu liste2D [] [] x1 y1 x2 y2 0
                            
                            if messageN == "non gobable"
                                then lancerPartiePourUnMove joueur moves (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
                                
                            else do 
                                --retirer en utilisant onboard ds la fonction gober pour le jeu 
                                --retirer en utilisant removeKey ici 
                                let pieceEtListDictio = removeKeyValueAtPosition x1 y1 taillePiece updatedList
                                case pieceEtListDictio of
                                  Just ((cleEnlevee, valeurEnlevee), liste2DUpdate) -> 
                                    lancerPartiePourUnMove joueurSuivant moves (ListeModifie jeuUpdate (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2DUpdate
                                  Nothing -> lancerPartiePourUnMove joueurSuivant moves (ListeModifie jeuUpdate (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D

    else lancerPartiePourUnMove joueur moves (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
--}



-- Fonction pour convertir un tableau 2D en String
tableau2DToString :: Show a => [[a]] -> String
tableau2DToString tableau = unlines $ map (unwords . map show) tableau



-- Fonction pour sauvegarder un tableau 2D dans un fichier
sauvegarderTableau2D :: Show a => [[a]] -> FilePath -> IO ()
sauvegarderTableau2D tableau nomFichier = do
    let donnees = tableau2DToString tableau
    appendFile nomFichier (donnees ++ "\n")

-- Fonction pour ajouter les données à un fichier existant en append
sauvegarder :: String -> FilePath -> IO ()
sauvegarder donnees nomFichier = appendFile nomFichier (donnees ++ "\n")


-- Fonction pour sauvegarder un tableau 2D et un message dans un fichier
sauvegarderTableauEtMessage :: Show a => [[a]] -> String -> FilePath -> IO ()
sauvegarderTableauEtMessage tableau message nomFichier = withFile nomFichier AppendMode $ \handle -> do
    hPutStrLn handle message
    hPutStrLn handle $ tableau2DToString tableau


-- | Cette fonction va lancer une partie ou chaque joueur va jouer en faisant un coup 
-- | Quand la partie est finit, cela montre l'état du jeu au dernier tour joué dans un fichier
lancerPartie :: Player -> Int -> ListeModifie -> [[DictionnairePiece]] -> IO ()
lancerPartie joueur tour (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D = do
    
    putStrLn ("tour " ++ show tour)

    let messageFin = quiAGagne joueur
    
    let (ListeCoord listPosPiecHumain listPosPiecOrd) = creerListeCoordPieces jeu [] [] 0 0

    let joueurActuel = determinerJoueurActuel joueur 
    
    let adversaire = voiciLeJoueurAdverse joueur
    
    let listePosPieceAdversaire = retourneListePieceAdversaire adversaire listPosPiecHumain listPosPiecOrd

    if estLeJoueurGagnant listePosPieceAdversaire 0
        then do 
            print messageFin
            let message = messageFin ++ " au Tour " ++ show tour
            sauvegarderTableauEtMessage jeu message "tableau2D.txt"
            sauvegarderTableau2D jeu "tableau2D.txt"
            
    else do 
        let joueurSuivant = alternerJoueurs joueur 
        let playerNow = quiJoueNow joueur
        print playerNow
        
        putStrLn "Informations de l'utilisateur :"
        userInfo <- resaisirCoup
    
        let parsedMoves = parseMoves userInfo
    
        let (taille, x1, y1, x2, y2) = case recupererDropOuOnboardInfo parsedMoves of
                Just (size, x1, y1, x2, y2) -> (size, x1, y1, x2, y2)
                Nothing -> (B, -1, -1, -1, -1)  -- Valeurs par défaut si pas de drop/onboard
        
        let tt = transformeSizeEnString taille
    
        let tonCoupEstValide = verifierCoup [x1,y1,x2,y2] 
        
        if tonCoupEstValide 
            then do 
                let jeuVide = estUnJeuVide jeu
                
                --cas drop
                --tab vide 
                if jeuVide
                    then do 
                        if estCasDrop [x2,y2] 
                            then do 
                                --verifie ordre des tailles avant de jouer ds le deck 
                                
                                let bonOrdre = piecePrisDansBonOrdre joueur tt (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1))
    
                                if bonOrdre == "oui"
                                    then do 
                                    
                                        let (ListeModifie jeuUpdate (ListesDispo (Deck Humain newdecHum) (Deck Ordi1 newDeckOrd1)) pieceAJouer2) = drop1 joueur jeu y1 x1 (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1)
                                        let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceAJouer2 liste2D
                                        
                                        printList2DDictio newListe2DDictio
                            
                                        putStrLn $ displayBoard jeuUpdate
                                        
                                        print (afficher2Decks newDeckOrd1 newdecHum)
                                        
                                        lancerPartie joueurSuivant (tour + 1) (ListeModifie jeuUpdate (ListesDispo (Deck Humain newdecHum) (Deck Ordi1 newDeckOrd1)) pieceAJouer2) newListe2DDictio
                                        
                                else do 
                                    
                                    putStrLn "cest pas un ordre aproprié, on recommence"
                                    lancerPartie joueur tour (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
    
                        --cas onboard     
                        else do 
                            putStrLn "tu peux pas déplacer de pieces, cest un tab vide"
                            lancerPartie joueur tour (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
                else do 
                    putStrLn "ook ca devient interressant, tab non vide"
                    
                    
                    if estCasDrop [x2,y2]
                        then do
                        
                            --verifie ordre des tailles avant de jouer ds le deck
                            let bonOrdre = piecePrisDansBonOrdre joueur tt (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1))
                            print decHum
                            print deckOrd1
                            print bonOrdre
                            if bonOrdre == "oui"
                                then do
                                    
                                    ----determiner pieces de adversaire
                                    ----determiner dabord qui est joueur actuel pour deduire qui est adversaire
                                    
                                    let (ListeCoord listePosPiecHumain listePosPiecOrd1) = creerListeCoordPieces jeu [] [] 0 0
                                    --string humain ou joueur
                                    let joueurActuel = determinerJoueurActuel joueur 
                                    
                                    let adversaire = voiciLeJoueurAdverse joueur

                                    let listePosPieceAdversaire = retourneListePieceAdversaire adversaire listePosPiecHumain listePosPiecOrd1
                                    
                                    let joueurAdversePossedAlign3 = existence1Align3 listePosPieceAdversaire []

                                    if joueurAdversePossedAlign3 == "non"
                                        then do
                                            
            -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                            let pieceACettePosition = retourePieceAUnePosition jeu y1 x1
                                            --cas drop case vide
                                            if estUneCaseVide pieceACettePosition
                                                then do
                                                    --let pieceACetteTaille = trace ("aaaaaa" ++ tt ++ message) $ retournePieceCorrespondante joueur tt (ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
                                                    
                                                    let (ListeModifie jeuUpdate (ListesDispo (Deck Humain newdecHum) (Deck Ordi1 newDeckOrd1)) pieceAJouer2) = drop1 joueur jeu y1 x1 (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1)
                                                    let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceAJouer2 liste2D
                    
                                                    printList2DDictio newListe2DDictio
                                                    putStrLn $ displayBoard jeuUpdate
                                                    print (afficher2Decks newDeckOrd1 newdecHum)

                                                    lancerPartie joueurSuivant (tour + 1) (ListeModifie jeuUpdate (ListesDispo (Deck Humain newdecHum) (Deck Ordi1 newDeckOrd1)) pieceAJouer2) newListe2DDictio
                                                        
                                            else do
                                            
                                                putStrLn "to bad case non vide, faut gober"
                                                --est une piece gobable?
                                                
                                                --recuere piece a cette taille tt 
                                                --je fais juste recuperer la piec de tt qui sera pieceACetteTaille
                                                let pieceACetteTaille = retournePieceCorrespondante joueur tt
                                                let (InfosJeu messageN jeuUpdate listeCoordJr11 listeCoordJr22 updatedList x y i) = goberUnePiece tt pieceACetteTaille jeu liste2D [] [] x1 y1 x1 y1 0
                                        
                                                if messageN == "non gobable"
                                                    then do
                                                        print messageN
                                                        putStrLn "tu ne peux pas gober cette piece, recommence"
                                                        lancerPartie joueur tour (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
                                                    
                                                else do 
                                                    let (ListeModifie jeuNew (ListesDispo (Deck Humain deckHum1) (Deck Ordi1 deckOrd1)) pieceAJouer2) = drop1 joueur jeuUpdate y1 x1 (ListeModifie jeuUpdate (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1)
                                                    let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceACetteTaille liste2D 
                                                    printList2DDictio newListe2DDictio
                                                    putStrLn $ displayBoard jeuNew
                                                    print (afficher2Decks deckOrd1 deckHum1)
                                                    
                                                    lancerPartie joueurSuivant (tour + 1) (ListeModifie jeuNew (ListesDispo (Deck Humain deckHum1) (Deck Ordi1 deckOrd1)) pieceAJouer2) newListe2DDictio
            -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                            
                                        
                                    else do 
                                        putStrLn "faut placer sur une des cases du jr adverse"
                                        --compare le xy de drop avec listePiecesJrAdvers
                                        --si vaJouerSurUneCaseAdverse listePiecesJrAdvers est True , joue normalement
                                        --sinon redemande en disant joueur actuel doit jouer sur case jr adverse
                                        
                                        --est-ce que joueurActuel va jouer sur une case du joueur adverse
                                        
                                        --remplacer ce if else par ma fonction peutFaireSonCoup joueur
                                        let reponse = peutFaireSonCoup joueurActuel [x1,y1] listePosPiecHumain listePosPiecOrd1
                                        --------------------------------------------------------------------
                                        
                                        if reponse == "noon"
                                            then do 
                                                putStrLn "tu dois obligatoirement jouer sur une case de l'adversaire, car il a un align de 3, on recommence"
                                                lancerPartie joueur tour (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
                                        else do 
                                            ---je peux jouer, alors je vais devoir gober la piece du joueur adverse 
                                            ---ici je sais deja que la case de cette piece est occupée donc as besoin de vérifier si la case est vide ou non
                                            let pieceACetteTaille = retournePieceCorrespondante joueur tt
                                            let (InfosJeu messageN jeuUpdate listeCoordJr11 listeCoordJr22 updatedList x y i) = goberUnePiece tt pieceACetteTaille jeu liste2D [] [] x1 y1 x1 y1 0
                                    
                                            if messageN == "non gobable"
                                                then do
                                                    print messageN
                                                    putStrLn "tu ne peux pas gober cette piece, recommence"
                                                    lancerPartie joueur tour (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
                                                
                                            else do 
                                                let (ListeModifie jeuNew (ListesDispo (Deck Humain deckHum1) (Deck Ordi1 deckOrd1)) pieceAJouer2) = drop1 joueur jeuUpdate y1 x1 (ListeModifie jeuUpdate (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1)
                                                let newListe2DDictio = addToDictionaryIn2DList x1 y1 tt pieceACetteTaille liste2D 
                                                printList2DDictio newListe2DDictio
                                                putStrLn $ displayBoard jeuNew
                                                print (afficher2Decks deckOrd1 deckHum1)
                                                lancerPartie joueurSuivant (tour + 1) (ListeModifie jeuNew (ListesDispo (Deck Humain deckHum1) (Deck Ordi1 deckOrd1)) pieceAJouer2) newListe2DDictio
    
                            else do 
                                putStrLn "l'ordre des pieces n'est pas respecté, recommence"
                                lancerPartie joueur tour (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
    
                    else do
                        --onboard((0, 2), (2, 1))
                        putStrLn "cas onboard tab pas vide hehe"
                        
                        let piecePositionDepart = retourePieceAUnePosition jeu y1 x1
                            --cas onboard case de depart
                        if estUneCaseVide piecePositionDepart
                            then do 
                                putStrLn "cas onboard tab as vide: recommence, tu peux pas déplacer car enplacement de départ vide"
                                lancerPartie joueur tour (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D 
                        else do 
                            -- no vide
                            --recupere taille de piece a cette position
                            
                            let taillePiece = recupereTailleCorrespondante piecePositionDepart 
    
                            let caseFinale = retourePieceAUnePosition jeu y2 x2
                            
                            if estUneCaseVide caseFinale
                                then do 
                                    print "ouiiiii case finale vide, onboard simple"
                                    let newJeu = onboard jeu x1 y1 x2 y2
                                    let newListe2DDictio = addToDictionaryIn2DList x2 y2 tt piecePositionDepart liste2D
                                    --retirerPiece de lancienne case (1,1)
                                    --let liste2DUpdate = removeKeyValueAtPosition x1 y1 taillePiece newListe2DDictio
                                    
                                    let pieceEtListDictio = removeKeyValueAtPosition x1 y1 taillePiece newListe2DDictio
        
                                    case pieceEtListDictio of
                                      Just ((cleEnlevee, valeurEnlevee), liste2DUpdate) -> do
                                        
                                        printList2DDictio liste2DUpdate
                                        putStrLn $ displayBoard newJeu
                                        print (afficher2Decks deckOrd1 decHum)
                                        lancerPartie joueurSuivant (tour + 1) (ListeModifie newJeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2DUpdate
                                        
                                      Nothing -> putStrLn "chaud"
                                    
                            else do 
                                print "piece a case finale a gober quand on deplace"
                                
                                let (InfosJeu messageN jeuUpdate listeCoordJr11 listeCoordJr22 updatedList x y i) = goberUnePiece taillePiece piecePositionDepart jeu liste2D [] [] x1 y1 x2 y2 0
                                
                                if messageN == "non gobable"
                                    then do
                                        putStrLn "tu ne peux pas gober cette piece, recommence"
                                        lancerPartie joueur tour (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
                                    
                                else do 
                                    --retirer en utilisant onboard ds la fonction gober pour le jeu 
                                    --retirer en utilisant removeKey ici 
                                    let pieceEtListDictio = removeKeyValueAtPosition x1 y1 taillePiece updatedList
                                    case pieceEtListDictio of
                                      Just ((cleEnlevee, valeurEnlevee), liste2DUpdate) -> do
                                        printList2DDictio liste2DUpdate
                                        putStrLn $ displayBoard jeuUpdate
                                        print (afficher2Decks deckOrd1 decHum)
                                    
                                        lancerPartie joueurSuivant (tour + 1) (ListeModifie jeuUpdate (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2DUpdate
                                      Nothing -> putStrLn "chaud"
                                      
        else do 
            putStrLn "Coup invalide , faut recommencer"
            lancerPartie joueur tour (ListeModifie jeu (ListesDispo (Deck Humain decHum) (Deck Ordi1 deckOrd1)) pieceAJouer1) liste2D
        
        
    
main :: IO ()
main = do

    let jeuDictioAvecPilePiece = liste2DAvecDictionnaire
    
    print "Debut de la partie"
    let initialListeModifie = ListeModifie jeu (ListesDispo (Deck Humain deckHumain) (Deck Ordi1 deckOrdi1)) ""
    lancerPartie Ordi1 0 initialListeModifie jeuDictioAvecPilePiece

    
