
{- |
Module      :  Joueur
Description :  Rassemble tout ce qui est relié a un joueur
Copyright   :  Oswald Essongué
License     :  <license>

Maintainer  :  cd891906@ens.uqam.ca
Stability   :  stable 
Portability :  portable 

-}

module Joueur where
import Debug.Trace
import Liste 

-- 
data ListeModifie = ListeModifie [[String]] ListesDispo String

-- Type de donnée Player qui peut etre un Humain Ou Un Ordi1
data Player 
    = Humain -- ^ Represente l'utilisateur.
    | Ordi1 -- ^ Represente l'IA.
    deriving (Eq, Show)

-- Type de donnée Deck qui est composé d'un Player et sa liste de pieces
data Deck = Deck Player [String]
    deriving (Show)

-- Type de donnée Deck qui est composé des decks des 2 joueurs
data ListesDispo = ListesDispo Deck Deck
    deriving (Show)


-- Deck de départ d'un Humain
deckHumain = ["X3", "X2", "X1", "X0","X3", "X2", "X1", "X0","X3", "X2", "X1", "X0"]

-- Deck de départ d'un Ordi
deckOrdi1 = ["O3", "O2", "O1", "O0","O3", "O2", "O1", "O0","O3", "O2", "O1", "O0"]


-- | Differentes tailles des pieces d'un joueur 
diffTaillePiece = ["B", "M", "S", "T"]


-- | Fonction pour alterner les joueurs lors d'une partie
alternerJoueurs :: Player -> Player
alternerJoueurs Humain = Ordi1
alternerJoueurs Ordi1 = Humain


-- | Fonction pour etablir le joueur gagnant 
quiAGagne :: Player -> String
quiAGagne Humain = "Vous avez gagné cette partie FELICITATIONS!!"
quiAGagne Ordi1 = "DOMMAGE l'ordinateur vous a battu"


-- | Fonction pour déterminer le joueur qui joue actuelllemnt 
quiJoueNow :: Player -> String
quiJoueNow Humain = "Moi"
quiJoueNow Ordi1 = "Ordi1"


 
-- | Fonction pour jouer un coup drop dans le jeu a partir du deck du joueur correspondant
drop1 :: Player -> [[String]] -> Int -> Int -> ListeModifie -> ListeModifie
drop1 joueur jeu x y (ListeModifie jeu1 (ListesDispo (Deck Humain deckH) (Deck Ordi1 deckO1)) pieceAJouer1) =
  let (pieceAJouer2, nouveauDeckHumain, nouveauDeckOrdi1) = case joueur of
        Humain -> (head deckH, tail deckH, deckO1)
        Ordi1 -> (head deckO1, deckH, tail deckO1)
      jeu1 = modifierCase2D jeu pieceAJouer2 x y
  in ListeModifie jeu1 (ListesDispo (Deck Humain nouveauDeckHumain) (Deck Ordi1 nouveauDeckOrdi1)) pieceAJouer2



-- | Fonction pour récupérer la taille correspondante d'une piece
recupereTailleCorrespondante :: String -> String
recupereTailleCorrespondante piece
    | piece == "X3" || piece == "O3" = "B"
    | piece == "X2" || piece == "O2" = "M"
    | piece == "X1" || piece == "O1" = "S"
    | piece == "X0" || piece == "O0" = "T"
    | otherwise = "Taille inconnue"


-- | Fonction pour vérifier si la piece que le joueur veut jouer est pris ds le bon ordre
piecePrisDansBonOrdre :: Player -> String -> ListesDispo -> String
piecePrisDansBonOrdre joueur taille (ListesDispo (Deck Humain deckH) (Deck Ordi1 deckO1))
  | joueur == Humain && taille `elem` diffTaillePiece && (head deckH) == retournePieceCorrespondante Humain taille = "oui"
  | joueur == Ordi1 && taille `elem` diffTaillePiece && (head deckO1) == retournePieceCorrespondante Ordi1 taille = "oui"
  | otherwise = "non"


-- | Fonction pour retourner la piece correspondant a la taille de celle-ci
retournePieceCorrespondante :: Player -> String -> String
retournePieceCorrespondante Humain "B" = "X3"
retournePieceCorrespondante Humain "M" = "X2"
retournePieceCorrespondante Humain "S" = "X1"
retournePieceCorrespondante Humain "T" = "X0"
retournePieceCorrespondante Ordi1 "B" = "O3"
retournePieceCorrespondante Ordi1 "M" = "O2"
retournePieceCorrespondante Ordi1 "S" = "O1"
retournePieceCorrespondante Ordi1 "T" = "O0"


-- | Fonction pour afficher les decks des 2 joueurs
afficher2Decks :: [String] -> [String] -> String
afficher2Decks list1 list2 =
  unwords list1 ++ " || " ++ unwords list2
