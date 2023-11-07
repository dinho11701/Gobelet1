module Main where

import State (State)
import State (displayState)
import Score (score)


-- Un état de jeu exemple
exampleState :: State
exampleState =
  [ ['O', 'X', 'X', '_']
  , ['X', 'O', '_', '_']
  , ['_', '_', 'O', '_']
  , ['_', '_', '_', '_']
  ]
  
depart :: State
depart = [ ['_', '_', '_', '_']
  , ['_', '_', '_', '_']
  , ['_', '_', '_', '_']
  , ['_', '_', '_', '_']
  ]

-- La fonction principale qui exécute le programme
main :: IO ()
main = do 
    print $ score exampleState
    --playGame depart
    
    --let initialState = emptyState  -- Vous devez définir votre propre état initial ici
    --let moveToMake = Drop B (Position 1 1)  -- Exemple de move (à personnaliser)
    --let newState = makeMove initialState moveToMake  -- Appelez makeMove avec l'état initial et le move
    putStrLn "After making the move:"
    putStrLn $ displayState exampleState 
    
    
    
    
