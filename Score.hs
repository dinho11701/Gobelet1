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
import Data.List (transpose, group)

-- | Calcule le score d'un état donné pour le jeu.
score :: State -> Int
score state = playerScore 'X' - playerScore 'O'
  where
    -- | Calcule le score d'un joueur dans un état donné en faisant la somme des scores pour chaque ligne et diagonale.
    playerScore player = sum $ map (calculateScoreForLine player) (linesAndDiagonals state)

    -- | Calcule le score d'un joueur pour une ligne donnée en comptant le nombre de séquences de 2 et 3 symboles consécutifs du joueur.
    calculateScoreForLine player line =
      let twoInARow = filter (\grp -> length grp == 2 && head grp == player) $ group line
          threeInARow = filter (\grp -> length grp == 3 && head grp == player) $ group line
      in length twoInARow + 10 * length threeInARow

    -- | Génère une liste de lignes et de diagonales à partir de l'état du jeu.
    linesAndDiagonals board = board ++ transpose board ++ diagonals board

    -- | Génère les diagonales principales (de gauche à droite) à partir de l'état du jeu.
    diagonals board = [primaryDiagonal board, primaryDiagonal (map reverse board)]

    -- | Génère la diagonale principale d'une matrice.
    primaryDiagonal [] = []
    primaryDiagonal (row:rows) = head row : primaryDiagonal (map tail rows)
