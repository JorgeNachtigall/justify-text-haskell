texto = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\nQuem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\naté o céu tudo entra na mesma sensação de propriedade.\n"

--firstLine :: String -> [String]
--firstLine [] = []
--firstLine [a] = [a]
--firstLine (a:b:x)
--   | a /= '\n' = a ++ firstLine (b:x)
--   | otherwise = 

separaLinhas :: String -> [String]
separaLinhas [] = []
separaLinhas a = [(takeWhile(/= '\n') a)] ++ separaLinhas (drop 1 (dropWhile(> '\n') a))

tamanhoMaiorLinha :: [Int] -> Int
tamanhoMaiorLinha [] = 0
tamanhoMaiorLinha (a:x) = 10

listaTamanhoLinhas :: [String] -> [Int]
listaTamanhoLinhas [] = []
listaTamanhoLinhas (a:x) = tamanhoLinha a : listaTamanhoLinhas x

tamanhoLinha :: String -> Int
tamanhoLinha [] = 0
tamanhoLinha (a:x) = 1 + tamanhoLinha x

