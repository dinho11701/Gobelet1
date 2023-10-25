{--import Move


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

--}
import System.IO
import Move 

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

main :: IO ()
main = do
    userInfo <- getUserInfo
    putStrLn "Informations de l'utilisateur :"
    putStrLn $ "Coup : " ++ head userInfo  -- Utilisez "head" pour accéder au premier élément de la liste

    let parsedMoves = parseMoves userInfo
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



