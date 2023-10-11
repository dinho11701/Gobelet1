import Move


main :: IO ()
main = do
  let moves = ["drop(B, (0, 1))", "onboard((0, 2), (2, 1))"]
      parsedMoves = parseMoves moves
  putStrLn "Mouvements analysés :"
  mapM_ print parsedMoves

  -- Récupération de la taille, x et y du premier drop s'il existe
  let firstDropInfo = case findDropInfo parsedMoves of
        Just (size, x, y) -> (size, x, y)
        Nothing -> (B, -1, -1)  -- Valeurs par défaut si pas de drop

  putStrLn "Info du premier drop :"
  print firstDropInfo
  
  let (taille, x, y) = firstDropInfo
  print taille



