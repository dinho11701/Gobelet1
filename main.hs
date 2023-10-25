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
    let (taille, x1, y1, x2, y2) = case recupererDropOuOnboardInfo parsedMoves of
            Just (size, x, y, x2, y2) -> (size, x, y, x2, y2)
            Nothing -> (B, -1, -1, -1, -1)  -- Valeurs par défaut si pas de drop/onboard

    putStrLn "Info du premier drop/onboard :"
    putStrLn $ "Taille : " ++ show taille
    putStrLn $ "x1 : " ++ show x1
    putStrLn $ "y1 : " ++ show y1
    putStrLn $ "x2 : " ++ show x2
    putStrLn $ "y2 : " ++ show y2


    
    
