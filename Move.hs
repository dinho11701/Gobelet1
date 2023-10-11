module Move where

import Text.Parsec
import Text.Parsec.String

data Size = T | S | M | B deriving Show
data Position = Position Int Int deriving Show
data Move = Drop Size Position | Onboard Position Position deriving Show

-- Analyseur pour les tailles
sizeParser :: Parser Size
sizeParser = choice
    [ T <$ char 'T'
    , S <$ char 'S'
    , M <$ char 'M'
    , B <$ char 'B'
    ]

-- Analyseur pour les positions
positionParser :: Parser Position
positionParser = do
    char '('
    x <- digit
    char ','
    spaces
    y <- digit
    char ')'
    return $ Position (read [x]) (read [y])

-- Analyseur pour les mouvements "onboard"
onboardParser :: Parser Move
onboardParser = do
    string "onboard"
    char '('
    position1 <- positionParser
    char ','
    spaces
    position2 <- positionParser
    char ')'
    return (Onboard position1 position2)

-- Analyseur pour les mouvements "drop"
dropParser :: Parser Move
dropParser = do
    string "drop"
    char '('
    size <- sizeParser
    char ','
    spaces
    position <- positionParser
    char ')'
    return (Drop size position)

-- Analyseur pour les mouvements
parseMoves :: [String] -> [Move]
parseMoves input = 
    case parse (many (try onboardParser <|> dropParser)) "" (unlines input) of
        Right moves -> moves
        Left _ -> []


-- Fonction pour extraire le x, y et la taille du premier drop
findDropInfo :: [Move] -> Maybe (Size, Int, Int)
findDropInfo [] = Nothing
findDropInfo (Drop size (Position x y) : _) = Just (size, x, y)
findDropInfo (_ : rest) = findDropInfo rest


