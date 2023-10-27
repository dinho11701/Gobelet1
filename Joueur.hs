module Joueur where

--ca me eprmet de renvoyer 2 listes (le jeu,la listeCoorespondante modifie du joueur)
data ListeModifie = ListeModifie [[String]] [String] String

data Player = Humain | Ordi1 | Ordi2 deriving (Eq)

diffTaillePiece = ["B", "M", "S", "T"]

data ListesDispo = ListesDispo String [String] [String] [String] [String] [String] [String] [String] [String]


-- Fonction pour alterner les joueurs
alternerJoueurs :: Player -> Player
alternerJoueurs Humain = Ordi1
alternerJoueurs Ordi1 = Humain


quiAGagne :: Player -> String
quiAGagne Humain = "Vous avez gagné YEES"
quiAGagne Ordi1 = "DOMMAGE VOUS AVEZ PERDU"



afficheJeuQuandOrdiFinit :: Player -> [[String]] -> IO()
afficheJeuQuandOrdiFinit Ordi1 jeu = print jeu




drop1 :: Player -> [[String]] -> String -> Int -> Int -> ListeModifie -> ListesDispo -> ListeModifie
drop1 joueur jeu taille x y (ListeModifie jeu1 listePieceDepart pieceAJouer1) (ListesDispo message listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
  | taille `elem` diffTaillePiece =
    let (pieces, diffPieceJoueur) = case joueur of
          Humain -> case taille of
            "B" -> (listeBU, listeBU)
            "M" -> (listeMU, listeMU)
            "S" -> (listeSU, listeSU)
            "T" -> (listeTU, listeTU)
            _   -> ([], ["pas bon"])
          Ordi1 -> case taille of
            "B" -> (listeBO, listeBO)
            "M" -> (listeMO, listeMO)
            "S" -> (listeSO, listeSO)
            "T" -> (listeTO, listeTO)
            _   -> ([], ["pas bon"])
        pieceAJouer = retirerTeteListe pieces
        newListePiece = prendResteListe diffPieceJoueur
        jeu1 = modifierCase2D jeu pieceAJouer x y
    in ListeModifie jeu1 newListePiece pieceAJouer
  | otherwise = ListeModifie [[]] ["pas bon"] ""


{--
drop1 :: Player -> [[String]] -> String -> Int -> Int -> ListeModifie -> ListesDispo -> (ListeModifie, ListesDispo)
drop1 joueur jeu taille x y (ListeModifie jeu1 newListePiece pieceAJouer) (ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
  | taille `elem` diffTaillePiece =
    let (pieces, diffPieceJoueur) = case joueur of
          Humain -> case taille of
            "B" -> (listeBU, listeBU)
            "M" -> (listeMU, listeMU)
            "S" -> (listeSU, listeSU)
            "T" -> (listeTU, listeTU)
            _   -> ([], ["pas bon"])
        in if length pieces == 0
             then ((ListeModifie jeu [] ""), (ListesDispo "choisis une autre catégorie" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO))
             else
               let pieceAJouer = retirerTeteListe pieces
                   newListePiece = prendResteListe diffPieceJoueur
                   jeu1 = modifierCase2D jeu pieceAJouer x y
                   listeDispoApres = case taille of
                     "B" -> ListesDispo "" newListePiece listeMU listeSU listeTU listeBO listeMO listeSO listeTO
                     "M" -> ListesDispo "" listeBU newListePiece listeSU listeTU listeBO listeMO listeSO listeTO
                     "S" -> ListesDispo "" newListePiece listeMU newListePiece listeTU listeBO listeMO listeSO listeTO
                     "T" -> ListesDispo "" newListePiece listeMU listeSU newListePiece listeBO listeMO listeSO listeTO
               in (ListeModifie jeu1 newListePiece pieceAJouer, listeDispoApres)
{--          Ordi1 -> case taille of
            "B" -> (listeBO, listeBO)
            "M" -> (listeMO, listeMO)
            "S" -> (listeSO, listeSO)
            "T" -> (listeTO, listeTO)
            _   -> ([], ["pas bon"])
        in if length pieces == 0
             then ((ListeModifie jeu [] ""), (ListesDispo "choisis une autre catégorie" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO))
             else
               let pieceAJouer = retirerTeteListe pieces
                   newListePiece = prendResteListe diffPieceJoueur
                   jeu1 = modifierCase2D jeu pieceAJouer x y
                   listeDispoApres = case taille of
                     "B" -> ListesDispo "" listeBU listeMU listeSU listeTU newListePiece listeMO listeSO listeTO
                     "M" -> ListesDispo "" listeBU listeMU listeSU listeTU listeBO newListePiece listeSO listeTO
                     "S" -> ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO newListePiece listeTO
                     "T" -> ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO newListePiece--}
               --in (ListeModifie jeu1 newListePiece pieceAJouer, listeDispoApres)--}
               

--retournePieceCorrespondante :: Player -> ListesDispo -> [String]
--retournePieceCorrespondante joueur (ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO) =



retournePieceCorrespondante :: Player -> String -> ListesDispo -> String
retournePieceCorrespondante Humain "B" (ListesDispo "" listeBU _ _ _ _ _ _ _) = 
    if not (null listeBU) then head listeBU else ""
retournePieceCorrespondante Humain "M" (ListesDispo "" _ listeMU _ _ _ _ _ _) = 
    if not (null listeMU) then head listeMU else ""
retournePieceCorrespondante Humain "S" (ListesDispo "" _ _ listeSU _ _ _ _ _) = 
    if not (null listeSU) then head listeSU else ""
retournePieceCorrespondante Humain "T" (ListesDispo "" _ _ _ listeTU _ _ _ _) = 
    if not (null listeTU) then head listeTU else ""
retournePieceCorrespondante Ordi1 "B" (ListesDispo "" _ _ _ _ listeBO _ _ _) = 
    if not (null listeBO) then head listeBO else ""
retournePieceCorrespondante Ordi1 "M" (ListesDispo "" _ _ _ _ _ listeMO _ _) = 
    if not (null listeMO) then head listeMO else ""
retournePieceCorrespondante Ordi1 "S" (ListesDispo "" _ _ _ _ _ _ listeSO _) = 
    if not (null listeSO) then head listeSO else ""
retournePieceCorrespondante Ordi1 "T" (ListesDispo "" _ _ _ _ _ _ _ listeTO) = 
    if not (null listeTO) then head listeTO else ""
retournePieceCorrespondante _ _ _ = []  -- Cas par défaut, retourne une liste vide



modifierListeDispo :: Player -> String -> [String] -> ListesDispo -> ListesDispo
modifierListeDispo joueur taille newListePiece (ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO)
  | joueur == Humain =
    case taille of
      "B" -> ListesDispo "" newListePiece listeMU listeSU listeTU listeBO listeMO listeSO listeTO
      "M" -> ListesDispo "" listeBU newListePiece listeSU listeTU listeBO listeMO listeSO listeTO
      "S" -> ListesDispo "" listeBU listeMU newListePiece listeTU listeBO listeMO listeSO listeTO
      "T" -> ListesDispo "" listeBU listeMU listeSU newListePiece listeBO listeMO listeSO listeTO
      _ -> error "Taille non valide pour Humain"
  | joueur == Ordi1 =
    case taille of
      "B" -> ListesDispo "" listeBU listeMU listeSU listeTU newListePiece listeMO listeSO listeTO
      "M" -> ListesDispo "" listeBU listeMU listeSU listeTU listeBO newListePiece listeSO listeTO
      "S" -> ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO newListePiece listeTO
      "T" -> ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO newListePiece
      _ -> error "Taille non valide pour Ordi1"
  | joueur == Ordi2 =
    case taille of
      "B" -> ListesDispo "" listeBU listeMU listeSU listeTU newListePiece listeMO listeSO listeTO
      "M" -> ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO listeTO
      _ -> error "Taille non valide pour Ordi2"
  | otherwise = error "Joueur non valide"


              {--Ordi1 -> case taille of
                "B" -> (listeBO, listeBO)
                "M" -> (listeMO, listeMO)
                "S" -> (listeSO, listeSO)
                "T" -> (listeTO, listeTO)
                _   -> ([], ["pas bon"])
            pieceAJouer = retirerTeteListe pieces
            newListePiece = prendResteListe diffPieceJoueur
            jeu1 = modifierCase2D jeu pieceAJouer x y
            case taille of 
                "B" -> ((ListeModifie jeu1 newListePiece pieceAJouer),(ListesDispo "" listeBU listeMU listeSU listeTU newListePiece listeMO listeSO listeTO))
                "M" -> ((ListeModifie jeu1 newListePiece pieceAJouer),(ListesDispo "" listeBU listeMU listeSU listeTU listeBO newListePiece listeSO listeTO))
                "S" -> ((ListeModifie jeu1 newListePiece pieceAJouer),(ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO newListePiece listeTO))
                "T" -> ((ListeModifie jeu1 newListePiece pieceAJouer),(ListesDispo "" listeBU listeMU listeSU listeTU listeBO listeMO listeSO newListePiece))--}
        



  
{--
drop1 :: Player -> String -> Int -> Int -> ListeModifie
drop1 Ordi1 taille x y
  | taille `elem` diffTaillePiece =
    let (pieces, diffPieceJoueur) = case taille of
          "B" -> (listeBComp, listeBComp)
          "M" -> (listeMComp, listeMComp)
          "S" -> (listeSComp, listeSComp)
          "T" -> (listeTComp, listeTComp)
          _   -> ([], ["pas bon"])
        pieceAJouer = retirerTeteListe pieces
        newListePiece = prendResteListe diffPieceJoueur
        jeu1 = modifierCase2D jeu pieceAJouer x y
    in ListeModifie jeu1 newListePiece
  | otherwise = ListeModifie [[]] ["pas bon"]
--}

--meilleur version , plus clean
modifierCase2D :: [[a]] -> a -> Int -> Int -> [[a]]
modifierCase2D tableau nouvelleValeur ligne colonne
  | ligne < 0 || ligne >= length tableau || colonne < 0 || colonne >= length (tableau !! 0) = tableau
  | otherwise =
    let (avant, apres) = splitAt colonne (tableau !! ligne)
    in take ligne tableau ++ [avant ++ [nouvelleValeur] ++ tail apres] ++ drop (ligne + 1) tableau


-- Définition des listes


listeUserBig = take 3 (repeat "X3")
listeUserMedium = take 3 (repeat "X2")
listeUserSmall = take 3 (repeat "X1")
listeUserTiny = take 3 (repeat "X0")


listeOrdiBig = take 3 (repeat "O3")
listeOrdiMedium = take 3 (repeat "O2")
listeOrdiSmall = take 3 (repeat "O1")
listeOrdiTiny = take 3 (repeat "O0")



listeBUser :: [String]
listeBUser = ["X3", "X2", "X1", "X0"]

listeMUser :: [String]
listeMUser = ["X3", "X2", "X1", "X0"]

listeSUser :: [String]
listeSUser = ["X3", "X2", "X1", "X0"]


listeTUser :: [String]
listeTUser = ["X3", "X2", "X1", "X0"]


listeBComp :: [String]
listeBComp = ["03", "02", "01", "00"]

listeMComp :: [String]
listeMComp = ["03", "02", "01", "00"]

listeSComp :: [String]
listeSComp = ["03", "02", "01", "00"]

listeTComp :: [String]
listeTComp = ["03", "02", "01", "00"]

formaterListeMov :: [[String]] -> String
formaterListeMov listes =
    let formatLigne ligne = unwords ligne
        formatListe liste = unwords liste
    in unwords [formatListe liste | liste <- listes]


--fonction qui va recuperer reste de la liste
prendResteListe :: [String] -> [String]
prendResteListe [] = []
prendResteListe(_:xs) =  xs 

--fonction qui va retirer l'element de la tete de la liste
retirerTeteListe :: [String] -> String
retirerTeteListe [] = []
retirerTeteListe(x:xs) = x 
