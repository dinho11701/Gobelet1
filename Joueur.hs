module Joueur where
import Debug.Trace

--ca me eprmet de renvoyer 2 listes (le jeu,la listeCoorespondante modifie du joueur)
data ListeModifie = ListeModifie [[String]] ListesDispo String

data Player = Humain | Ordi1 | Ordi2
    deriving (Eq, Show)

data Deck = Deck Player [String]
    deriving (Show)

data ListesDispo = ListesDispo Deck Deck
    deriving (Show)


deckHumain = ["X3", "X2", "X1", "X0","X3", "X2", "X1", "X0","X3", "X2", "X1", "X0"]
deckOrdi1 = ["O3", "O2", "O1", "O0","O3", "O2", "O1", "O0","O3", "O2", "O1", "O0"]


--type PiecesJoueur = [String] 
--type PiecesOrdi1 = [String]
--type PiecesJoueur2 = [String] 
--type PiecesOrdi2 = [String] 


--data Deck = Deck Player [String] deriving (Show)


diffTaillePiece = ["B", "M", "S", "T"]

--data ListesDispo = ListesDispo Deck Deck


-- Fonction pour alterner les joueurs
alternerJoueurs :: Player -> Player
alternerJoueurs Humain = Ordi1
alternerJoueurs Ordi1 = Humain


quiAGagne :: Player -> String
quiAGagne Humain = "Vous avez gagné YEES"
quiAGagne Ordi1 = "DOMMAGE VOUS AVEZ PERDU"


quiJoueNow :: Player -> String
quiJoueNow Humain = "Moi"
quiJoueNow Ordi1 = "Ordi1"




afficheJeuQuandOrdiFinit :: Player -> [[String]] -> IO()
afficheJeuQuandOrdiFinit Ordi1 jeu = print jeu



 
drop1 :: Player -> [[String]] -> Int -> Int -> ListeModifie -> ListeModifie
drop1 joueur jeu x y (ListeModifie jeu1 (ListesDispo (Deck Humain deckH) (Deck Ordi1 deckO1)) pieceAJouer1) =
  let (pieceAJouer2, nouveauDeckHumain, nouveauDeckOrdi1) = case joueur of
        Humain -> (head deckH, tail deckH, deckO1)
        Ordi1 -> (head deckO1, deckH, tail deckO1)
      jeu1 = modifierCase2D jeu pieceAJouer2 x y
  in ListeModifie jeu1 (ListesDispo (Deck Humain nouveauDeckHumain) (Deck Ordi1 nouveauDeckOrdi1)) pieceAJouer2




recupereTailleCorrespondante :: String -> String
recupereTailleCorrespondante piece
    | piece == "X3" || piece == "O3" = "B"
    | piece == "X2" || piece == "O2" = "M"
    | piece == "X1" || piece == "O1" = "S"
    | piece == "X0" || piece == "O0" = "T"
    | otherwise = "Taille inconnue"



piecePrisDansBonOrdre :: Player -> String -> ListesDispo -> String
piecePrisDansBonOrdre joueur taille (ListesDispo (Deck Humain deckH) (Deck Ordi1 deckO1))
  | joueur == Humain && taille `elem` diffTaillePiece && (head deckH) == retournePieceCorrespondante Humain taille = "oui"
  | joueur == Ordi1 && taille `elem` diffTaillePiece && (head deckO1) == retournePieceCorrespondante Ordi1 taille = "oui"
  | otherwise = "non"


retournePieceCorrespondante :: Player -> String -> String
retournePieceCorrespondante Humain "B" = "X3"
retournePieceCorrespondante Humain "M" = "X2"
retournePieceCorrespondante Humain "S" = "X1"
retournePieceCorrespondante Humain "T" = "X0"
retournePieceCorrespondante Ordi1 "B" = "O3"
retournePieceCorrespondante Ordi1 "M" = "O2"
retournePieceCorrespondante Ordi1 "S" = "O1"
retournePieceCorrespondante Ordi1 "T" = "O0"






        




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
