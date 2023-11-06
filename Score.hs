module Score where
import Liste 


creerListPieces2JoueursAvecDictio :: [[String]] -> Int -> Int -> [Int] -> [Int] -> [[DictionnairePiece]] ->ListeCoord
creerListPieces2JoueursAvecDictio jeu1 i j pieceJr pieceCom dictio2D = do 
    if i < length jeu1 
        then do -- Vérification de la limite en i
            if j < length jeu1
                then do 
                    let piece = recupererCleTeteDictio i j dictio2D
                    case piece of
                        Just piece -> do
                            if piece == "X3" || piece == "X2" || piece == "X1" || piece == "X0"
                                then do 
                                    let newPieceHum = ajouteElementALaFin pieceJr j
                                        newUpdateHum = ajouteElementALaFin newPieceHum i
                                    creerListPieces2JoueursAvecDictio jeu1 i (j + 1) newUpdateHum pieceCom dictio2D
                            else if piece `elem` ["O3","O2","O1","O0"]
                                then do 
                                    let newPieceCom = ajouteElementALaFin pieceCom j
                                        newUpdateCom = ajouteElementALaFin newPieceCom i
                                    creerListPieces2JoueursAvecDictio jeu1 i (j + 1) pieceJr newUpdateCom dictio2D
                            else creerListPieces2JoueursAvecDictio jeu1 i (j + 1) pieceJr pieceCom dictio2D
                        Nothing -> creerListPieces2JoueursAvecDictio jeu1 i (j + 1) pieceJr pieceCom dictio2D
            else creerListPieces2JoueursAvecDictio jeu1 (i + 1) 0 pieceJr pieceCom dictio2D
                
    else ListeCoord pieceJr pieceCom
    
    
    
creerListPieces2JoueursAvecDictioPrint :: [[String]] -> Int -> Int -> [Int] -> [Int] -> [[DictionnairePiece]] -> IO ()
creerListPieces2JoueursAvecDictioPrint jeu1 i j pieceJr pieceCom dictio2D = do 
    if i < length jeu1 
        then do -- Vérification de la limite en i
            if j < length jeu1
                then do 
                    let piece = recupererCleTeteDictio i j dictio2D
                    case piece of
                        Just piece -> do
                            print piece
                            if piece == "X3" || piece == "X2" || piece == "X1" || piece == "X0"
                                then do 
                                    let newPieceHum = ajouteElementALaFin pieceJr j
                                        newUpdateHum = ajouteElementALaFin newPieceHum i
                                    creerListPieces2JoueursAvecDictioPrint jeu1 i (j + 1) newUpdateHum pieceCom dictio2D
                            else if piece `elem` ["O3","O2","O1","O0"]
                                then do 
                                    let newPieceCom = ajouteElementALaFin pieceCom j
                                        newUpdateCom = ajouteElementALaFin newPieceCom i
                                    creerListPieces2JoueursAvecDictioPrint jeu1 i (j + 1) pieceJr newUpdateCom dictio2D
                            else creerListPieces2JoueursAvecDictioPrint jeu1 i (j + 1) pieceJr pieceCom dictio2D
                        Nothing -> creerListPieces2JoueursAvecDictioPrint jeu1 i (j + 1) pieceJr pieceCom dictio2D
            else creerListPieces2JoueursAvecDictioPrint jeu1 (i + 1) 0 pieceJr pieceCom dictio2D
                
    else do 
        print pieceJr
        print pieceCom
    
