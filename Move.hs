
{- |
Module      :  Move
Description :  Définit les types de taille, de position et de mouvement pour un jeu.
                Ce module inclut également des analyseurs pour interpréter la taille 
                et le mouvement ainsi que des fonctions utilitaires pour la validité
                des mouvements et la récupération d'informations.
Copyright   :  Oswald Essongué
License     :  <license>

Maintainer  :  cd891906@ens.uqam.ca
Stability   :  stable 
Portability :  portable 


Le module `Move` fournit les types de données fondamentaux et les fonctionnalités 
pour représenter et manipuler les mouvements dans un jeu. Il définit `Size` pour 
représenter différentes tailles de pièces de jeu, `Position` pour représenter les 
coordonnées sur le plateau de jeu, et `Move` pour représenter les actions de placement
ou de déplacement d'une pièce sur le plateau.

Des analyseurs syntaxiques sont inclus qui peuvent interpréter des chaînes de 
caractères comme des types de données `Size` et `Move`, permettant ainsi une 
interface basée sur le texte ou une communication réseau pour piloter la mécanique du jeu.

Des fonctions utilitaires sont fournies pour vérifier la validité d'un mouvement,
déterminer si un mouvement est un placement ou un déplacement d'une pièce, et pour 
extraire les informations pertinentes des mouvements.

-}



module Move where

import Text.Parsec
import Text.Parsec.String

data Size 
  
  = T  -- ^ Represente la taille la plus petite, T pour Tiny.
  | S  -- ^ Represente la taille small S.
  | M  -- ^ Represents la taille medium M .
  | B  -- ^ Represents la taille Big. 
  deriving Show
  
-- | Type de donnée pour représenter la position dans un jeu.
data Position
  = Position Int Int -- ^ Constructeur de x et y
  deriving Show
  
data Move = Drop Size Position -- ^ Mouvement drop pour placer la piece de a une position
    | Onboard Position Position -- ^ Mouvement onboard pour déplacer la piece d'une position a une autre
    deriving Show

-- | Analyseur pour les tailles
sizeParser :: Parser Size
sizeParser = choice
    [ T <$ char 'T'
    , S <$ char 'S'
    , M <$ char 'M'
    , B <$ char 'B'
    ]

-- | Analyseur pour les positions
positionParser :: Parser Position
positionParser = do
    char '('
    x <- digit
    char ','
    spaces
    y <- digit
    char ')'
    return $ Position (read [x]) (read [y])

-- | Analyseur pour les mouvements "onboard"
onboardParser :: Parser Move
onboardParser = do
    string "onboard"
    char '('
    position1 <- positionParser
    char ','
    optional spaces
    position2 <- positionParser
    char ')'
    return (Onboard position1 position2)

-- | Analyseur pour les mouvements "drop"
dropParser :: Parser Move
dropParser = do
    string "drop"
    char '('
    size <- sizeParser
    char ','
    optional spaces
    position <- positionParser
    char ')'
    return (Drop size position)

-- | Analyseur pour les mouvements
parseMoves :: [String] -> [Move]
parseMoves input = 
    case parse (many (try onboardParser <|> dropParser)) "" (unlines input) of
        Right moves -> moves
        Left _ -> []


-- | Transforme une taille en string 
transformeSizeEnString :: Size -> String
transformeSizeEnString T = "T"
transformeSizeEnString B = "B"
transformeSizeEnString S = "S"
transformeSizeEnString M = "M"


-- | Fonction pour extraire le ou les x, y et la taille dependamment de si cest un drop ou onboard
recupererDropOuOnboardInfo :: [Move] -> Maybe (Size, Int, Int, Int, Int)
recupererDropOuOnboardInfo [] = Nothing
recupererDropOuOnboardInfo (Drop size (Position x y) : _) = Just (size, x, y,-1,-1)
recupererDropOuOnboardInfo (Onboard (Position x1 y1) (Position x2 y2) : _) = Just (B,x1,y1,x2, y2)


-- | Détermine si un coup est valide 
estUnCoupValide :: Int -> Int -> Bool
estUnCoupValide x y = x >= 0 && x <= 3 && y >= 0 && y <= 3 


-- | Vérifie qu'un coup est valide ou non
verifierCoup :: [Int] -> Bool
verifierCoup liste
    | all(\x -> x == -1) liste = False
    | all(\x -> x == -1) [(liste !! 2),(liste !! 3)] = True
    | otherwise = True
    

-- | Détermine si le coup joué est un drop
estCasDrop :: [Int] -> Bool
estCasDrop liste = all(\x -> x == -1) liste 

