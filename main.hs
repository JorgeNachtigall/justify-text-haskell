texto = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\nQuem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\naté o céu tudo entra na mesma sensação de propriedade.\n"

justify :: String -> String
justify s = rebuild justify2

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

-- Returns a list of a list of words

wordsList :: [String] -> [[String]]
wordsList [] = []
wordsList (a:x) = splitWords a : wordsList x

-- Adds an space in a word

addSpace :: String -> Int -> String
addSpace s 0 = s
addSpace s n
   | n > 0 = addSpace s (n-1) ++ " "

-- Adds spaces in the words based on the map list

increaseWords :: [[String]] -> [[Int]] -> [[String]]
increaseWords3 [a] l = [a]
increaseWords3 (a:x) (w:z) = (increaseWords4 a w) : increaseWords3 x z

increaseWords :: [String] -> [Int] -> [String]
increaseWords4 [] l = []
increaseWords4 (a:x) (w:z) = (addSpace a w) : increaseWords4 x z

-- Returns a list where each element is a word from the text

splitWords :: String -> [String]
splitWords [] = []
splitWords a = [(takeWhile(\x -> (x /= ' ') && (x /= '\n')) a)] ++ splitWords (drop 1 (dropWhile(> ' ') a))

-- Returns a list with the number of words in a line

wordsPerLine :: [[String]] -> [Int]
wordsPerLine [] = []
wordsPerLine (a:x) = wordsPerLine2 a : wordsPerLine x

wordsPerLine2 :: [String] -> Int
wordsPerLine2 [] = 0
wordsPerLine2 (a:x) = 1 + wordsPerLine2 x

-- Create a list to map the space adition in a line

mapSpaces :: [Int] -> [Int] -> [[Int]]
mapSpaces [] l = []
mapSpaces (a:x) (w:z) = mapSpaces2 a w 1 : mapSpaces x z

mapSpaces2 :: Int -> Int -> Int -> [Int]
mapSpaces2 0 words cont = []
mapSpaces2 spacesNumber words cont
   | spacesNumber > 0 && cont < words = cont : mapSpaces2 (spacesNumber-1) words (cont+1)
   | cont == words = mapSpaces2 (spacesNumber) words 1

-- Transform the map list to a ocurrency list

mapToOcurr :: [[Int]] -> [[Int]] -> [[Int]]
mapToOcurr [] l = []
mapToOcurr (a:x) (w:z) = mapToOcurr2 a w : mapToOcurr x z

mapToOcurr2 :: [Int] -> [Int] -> [Int]
mapToOcurr2 [] l = []
mapToOcurr2 (a:x) l = mapToOcurr3 a l : mapToOcurr2 x (removeElement a l)

mapToOcurr3 :: Int -> [Int] -> Int
mapToOcurr3 n [] = 0
mapToOcurr3 a (w:z)
   | a == w = 1 + mapToOcurr3 a z
   | otherwise = mapToOcurr3 a z

removeElement :: Int -> [Int] -> [Int]
removeElement n [] = []
removeElement n (a:x)
   | n == a = removeElement n x
   | otherwise = a : removeElement n x

-- Fix the ocurrency list (the ocurrency list size needs to be at least the same of the numbers of the words of each line)

fixMap :: [[Int]] -> [Int] -> [[Int]]
fixMap [] l = []
fixMap (a:x) (w:z) = fixMap2 a w : fixMap x z
   
fixMap2 :: [Int] -> Int -> [Int]
fixMap2 x w = x ++ fixMap3 (abs (w - (countElements x)))

fixMap3 :: Int -> [Int]
fixMap3 0 = []
fixMap3 n = 0 : fixMap3 (n-1)

-- Rebuild the message

rebuild :: [[String]] -> String
rebuild [] = []
rebuild (a:x) = rebuild2 a ++ "\n" ++ rebuild x

rebuild2 :: [String] -> String
rebuild2 [] = []
rebuild2 (a:x) = a ++ " " ++ rebuild2 x

-- Count elements of an list

countElements :: [Int] -> Int
countElements [] = 0
countElements (a:x) = 1 + countElements x

-- Returns a list with the number of blank spaces needed to be added in each line

blankSpaceNeeded :: [String] -> Int -> [Int]
blankSpaceNeeded [] n = []
blankSpaceNeeded (a:x) n = (n - lineSize a) : blankSpaceNeeded x n

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

wordsAll = wordsList linesAll

mapFunc = mapSpaces blankSpaceList (wordsPerLine (wordsList linesAll))

mapConvert = mapToOcurr mapFunc mapFunc

linesAll = splitLines texto

blankSpaceList = blankSpaceNeeded linesAll longestLineSize

longestLineSize = sizeLongestLine (lineSizeList (splitLines texto))

fixList = fixMap mapConvert (wordsPerLine (wordsList linesAll))

justify2 = increaseWords3 wordsAll fixList