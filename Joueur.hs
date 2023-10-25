module Joueur where

--ca me eprmet de renvoyer 2 listes (le jeu,la listeCoorespondante modifie du joueur)
data ListeModifie = ListeModifie [[String]] [String] String



data Player = Humain | Ordi1 | Ordi2

diffTaillePiece = ["B", "M", "S", "T"]

data ListesDispo = listesDispo [String] [String] [String] [String] [String] [String]

drop1 :: Player -> [[String]] -> String -> Int -> Int -> ListeModifie
drop1 joueur jeu taille x y
  | taille `elem` diffTaillePiece =
    let (pieces, diffPieceJoueur) = case joueur of
          Humain -> case taille of
            "B" -> (listeBUser, listeBUser)
            "M" -> (listeMUser, listeMUser)
            "S" -> (listeSUser, listeSUser)
            "T" -> (listeTUser, listeTUser)
            _   -> ([], ["pas bon"])
          Ordi1 -> case taille of
            "B" -> (listeBComp, listeBComp)
            "M" -> (listeMComp, listeMComp)
            "S" -> (listeSComp, listeSComp)
            "T" -> (listeTComp, listeTComp)
            _   -> ([], ["pas bon"])
        pieceAJouer = retirerTeteListe pieces
        newListePiece = prendResteListe diffPieceJoueur
        jeu1 = modifierCase2D jeu pieceAJouer x y
    in ListeModifie jeu1 newListePiece pieceAJouer
  | otherwise = ListeModifie [[]] ["pas bon"] ""

  
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
