texto = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\nQuem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\naté o céu tudo entra na mesma sensação de propriedade.\n"

-- Returns a list where each element is a line from the text

splitLines :: String -> [String]
splitLines [] = []
splitLines a = [(takeWhile(/= '\n') a)] ++ splitLines (drop 1 (dropWhile(> '\n') a))

-- Returns the size of the longest line

sizeLongestLine :: [Int] -> Int
sizeLongestLine l = let l2 = iSort l
                    in lastElement l2

-- Returns a list where each element is the size of an line

lineSizeList :: [String] -> [Int]
lineSizeList [] = []
lineSizeList (a:x) = lineSize a : lineSizeList x

-- Returns the size of an line

lineSize :: String -> Int
lineSize [] = 0
lineSize (a:x) = 1 + lineSize x

-- Split the list of lines in a list of words

listOfWords :: [String] -> [[String]]
listOfWords [] = []
-- TODO: listOfWords (a:x) = [[takeWhile(/= ' ') a]] : [[listOfWords (dro)]]


-- Returns the number of blank spaces needed to be added in a line

blankSpaceNeeded :: String -> Int -> Int
blankSpaceNeeded s n = n - lineSize s

-- List sorting functions

insert :: Int -> [Int] -> [Int]
insert a [] = [a]
insert a (b:x)
   | a <= b = a:(b:x)
   | otherwise = b : insert a x

iSort :: [Int] -> [Int]
iSort [] = []
iSort (a:x) = insert a (iSort x)

lastElement :: [Int] -> Int
lastElement [a] = a
lastElement (a:x) = lastElement x

-------------------------------

longestLineSize = sizeLongestLine (lineSizeList (splitLines texto))