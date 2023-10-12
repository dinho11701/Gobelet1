module Joueur where
    

-- Définition des listes
listeBUser :: [String]
listeBUser = ["X3", "X2", "X1", "X0"]

listeMUser :: [String]
listeMUser = ["X3", "X2", "X1", "X0"]

listeSUser :: [String]
listeSUser = ["X3", "X2", "X1", "X0"]


listeTUser :: [String]
listeTUser = ["X3", "X2", "X1", "X0"]


listeBComp :: [String]
listeBComp = ["03", "02", "01", "00"]

listeMComp :: [String]
listeMComp = ["03", "02", "01", "00"]

listeSComp :: [String]
listeSComp = ["03", "02", "01", "00"]

listeTComp :: [String]
listeTComp = ["03", "02", "01", "00"]

formaterListeMov :: [[String]] -> String
formaterListeMov listes =
    let formatLigne ligne = unwords ligne
        formatListe liste = unwords liste
    in unwords [formatListe liste | liste <- listes]


--fonction qui va recuperer reste de la liste
prendResteListe :: [String] -> [String]
prendResteListe [] = []
prendResteListe(_:xs) =  xs 

--fonction qui va retirer l'element de la tete de la liste
retirerTeteListe :: [String] -> String
retirerTeteListe [] = []
retirerTeteListe(x:xs) = x 
