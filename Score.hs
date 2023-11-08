{- |
Module      :  Score
Description :  Calcule le score d'un State
Copyright   :  Oswald Essongué
License     :  <license>

Maintainer  :  cd891906@ens.uqam.ca
Stability   :  stable 
Portability :  portable 

-}


module Score (score) where

import State (State)
import Data.List (transpose, inits, tails)

-- | Vérifie si une séquence peut potentiellement former une ligne de 4.
-- Une séquence est un alignement potentiel si elle contient au moins une pièce d'un joueur et peut être étendue à une longueur de 4.
alignementPotentiel :: Char -> String -> Bool
alignementPotentiel joueur seq =
  joueur `elem` seq && length seq == 4 && not (all (/= joueur) seq)

-- | Aplatit une liste de listes en une liste de chaînes, où chaque chaîne représente une ligne potentielle.
aplatirInitsTails :: [[a]] -> [[a]]
aplatirInitsTails = concatMap (\xs -> concatMap tails (inits xs))

-- | Calcule le score d'un état donné pour le jeu.
score :: State -> Int
score etat = scoreJoueur 'X' - scoreJoueur 'O'
  where
    scoreJoueur joueur = sum $ map (scoreLigne joueur) (lignesEtDiagonales etat)

    -- | Calcule le score d'une ligne en considérant les alignements potentiels.
    scoreLigne joueur ligne =
      let sequences = aplatirInitsTails ligne
          alignementsPotentiels = filter (alignementPotentiel joueur) sequences
      in sum $ map (scoreSequence joueur) alignementsPotentiels

    -- | Calcule le score d'une séquence en considérant les alignements partiels.
    scoreSequence joueur seq =
      let compteJoueur = length $ filter (== joueur) seq
      in case compteJoueur of
          1 -> 1
          2 -> 5
          3 -> 10
          _ -> 0

    -- | Génère une liste de lignes et de diagonales à partir de l'état du jeu.
    lignesEtDiagonales plateau = plateau ++ transpose plateau ++ diagonales plateau

    -- | Génère les diagonales principales (de gauche à droite) à partir de l'état du jeu.
    diagonales plateau = [diagonalePrincipale plateau, diagonalePrincipale (map reverse plateau)]

    -- | Génère la diagonale principale d'une matrice.
    diagonalePrincipale [] = []
    diagonalePrincipale (rangee:rangees) = head rangee : diagonalePrincipale (map tail rangees)
