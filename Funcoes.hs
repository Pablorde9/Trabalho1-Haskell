module Funcoes where

import Tipos
import Data.Time.Calendar
import Data.Char (isDigit)

-- *os nomes das funcoes segue a estrutura nomeFuncao e de variaveis nome_variavel, pra nao ter confusao*
-- TODO: implementar nas funções o Either para casos de erro

-- FUNCOES BASICAS:


-- funcao auxiliar que retorna a lista de todos os ids de uma lista
todosIds :: [Tarefa] -> [Int] 
todosIds lista_tarefa = map idTarefa lista_tarefa


--checa se o id de uma tarefa ja pertence a uma lista
idPertenceLista :: Tarefa -> [Tarefa] -> Bool
idPertenceLista tarefa lista_tarefa = (elem (idTarefa tarefa) (todosIds lista_tarefa)) 

-- adiciona elementos a lista
adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]
adicionarTarefa tarefa lista_tarefa = 
     if (idPertenceLista tarefa lista_tarefa) -- compara se o id da nova tarefa ja pertence a lista
       then lista_tarefa -- se o id da nova tarefa ja existir na lista, retorna a mesma lista
       else (tarefa : lista_tarefa) -- coloca a nova tarefa no comeco da lista de tarefas


-- verifica se existe algum id(t) que seja igual ao id fornecido, e o remove
-- TODO: adicionar um indicador para se a tarefa foi encontrada ou removida
removerTarefa :: Int -> [Tarefa] -> [Tarefa]
removerTarefa id lista_tarefa = filter (\t -> idTarefa t /= id) lista_tarefa  


-- mapeia por todos elementos ate encontrar o id fornecido, quando encontra muda o status dele para concluido
-- se não encontrar o elemento, nada é alterado
-- TODO: adicionar uma forma de verificar se a tarefa realmente exitse 
marcarConcluida :: Int -> [Tarefa] -> [Tarefa]
marcarConcluida id = map (\t -> if idTarefa t == id then t { status = Concluida } else t)




--FUNÇÕES AVANÇADAS


-- lista a categoria com filter aplicada junto com a lista fornecida
listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria categoria_listada = filter(\t -> categoria t == categoria_listada)


-- filtra por prioridade fornecida
listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade prioridade_listada = filter (\t -> prioridade t == prioridade_listada)


-- filtra pelo status fornecido
filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
filtrarPorStatus status_listado = filter (\t -> status t == status_listado)


-- ordena os itens da lista por prioridade (do menor ao maior)
ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
ordenarPorPrioridade [] = []
ordenarPorPrioridade (x:xs) = ordenarPorPrioridade maiores ++ [x] ++ ordenarPorPrioridade menores
   where
      --retira os elementos que são menores que x e filtra no resto da lista, deixando apenas os maiores que x
      maiores = filter (\t -> prioridade t > prioridade x) (xs)
      
      --retira os maiores e iguais a x e filtra no restoda lista, deixando apenas os menores que x 
      menores = filter (\t -> prioridade t <= prioridade x) (xs)
      

--filtra a lista com base na palavra procurada
buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
buscarPorPalavraChave palavra lista_tarefas = filter (\t -> contem palavra (descricao t)) lista_tarefas
  where
    --verifica se contem a palavra, chamado a funcao corresponde para verificar
    contem _ [] = False
    contem [] _ = True
    contem p s = corresponde p s || contem p (avancar s)

    --verifica se o elemento contido corresponde a palavra procurada na lista
    corresponde [] _ = True
    corresponde _ [] = False
    corresponde (p:ps) (s:ss) = p == s && corresponde ps ss
    

     --avança em um elemento da lista
    avancar (_:xs) = xs
    avancar [] = []

-- Funcoes de Gestao de prazos:


--verifica se uma tarefa esta em atraso de forma recursiva, devolvendo apenas tais tarefas
verificarAtrasos :: [Tarefa] -> Day -> [Tarefa]
verificarAtrasos [] _ = []
verificarAtrasos (t:ts) dataAtual
--p recebe o valor prazo da tarfea t e compara com a data fornecida, e comprara se t tambem está pendente
  | Just p <- prazo t, p < dataAtual, status t == Pendente = t : verificarAtrasos ts dataAtual
--caso nao esteja em atraso a lista continua a checar o resto dela, tarefas sem prazo nunca são consideradas em atraso
  | otherwise = verificarAtrasos ts dataAtual

--faz o calculo dos dias restantes para todas as tarefas que tenham prazo
calcularDiasRestantes :: Tarefa -> Data -> Maybe Int
calcularDiasRestantes tarefa dataAtual =
  case prazo tarefa of
    Nothing -> Nothing --tarefa sem prazo definido
    Just p -> Just (diferencaDias p dataAtual) --calcula dias restantes

-- Função auxiliar para calcular diferença entre datas
diferencaDias :: Data -> Data -> Int
diferencaDias (Data a1 m1 d1) (Data a2 m2 d2) =
  (a1 - a2) * 360 + (m1 - m2) * 30 + (d1 - d2) --aproximação de mês para 30 dias




-- Funcoes de Sistema de Tags:


-- funcao que recebe uma tag alvo e uma lista de tarefas e retorna uma lista com todas as tarefas que possuem tal tag
filtrarPorTag :: String -> [Tarefa] -> [Tarefa]
filtrarPorTag tag_alvo lista_tarefa = filter (\t -> contem tag_alvo (tags t)) lista_tarefa -- filter aplicado sobre a lista e com a funcao auxiliar "contem"
  where
  -- funcao auxiliar recursiva que checa se a tag dada e igual a alguma das tags da tarefa
  contem _ [] = False -- se a lista de tags da tarefa esta vazia, entao e falso
  contem [] _ = True -- se a tag desejada e vazia, entao retorna todas as tarefas
  contem tag_alvo (x:xs) = if x == tag_alvo then True else contem tag_alvo xs -- se a a tag e igual retorna True, senao testa a proxima tag


-- funcao que recebe uma tag alvo e uma lista de tags, retorna uma nova lista de tags sem nenhuma instancia da tag alvo 
excluiTagsRepetidas :: String -> [String] -> [String]
excluiTagsRepetidas tag_alvo lista_tags = (filter (\t -> t /= tag_alvo) lista_tags) -- filter que retorna true quando a tag da lista e diferente da tag alvo


-- recebe uma lista de tags e remove todas as tags repetidas, retorna uma lista com apenas uma instancia de cada tag da lista original 
listaDeTagsExclusivas :: [String] -> [String]
listaDeTagsExclusivas [] = [] -- caso base
listaDeTagsExclusivas (x:xs) = [x] ++ listaDeTagsExclusivas (excluiTagsRepetidas x xs) -- retorna a tag a ser analisada ++ a lista sem nenhuma instancia dela


-- recebe uma tag alvo e uma lista de tags, retorna quantas vezes a tag alvo aparece ao longo da lista
contagemTagsIguais :: String -> [String] -> Int
contagemTagsIguais [] _ = 0 -- caso a tag alvo seja vazia, nao tem correspondencia
contagemTagsIguais _ [] = 1 -- quando a lista fica sem elementos retorna 1, que se refere a instancia original do elemento na lista
contagemTagsIguais tag_alvo (x:xs) = if tag_alvo == x then 1 + contagemTagsIguais tag_alvo xs else contagemTagsIguais tag_alvo xs  -- acha de forma recursiva se o elemento se repete


-- funcao que recebe uma lista de tarefas e retorna uma lista com as tags presentes e sua frequencia de suso
nuvemDeTags :: [Tarefa] -> [(String, Int)]
nuvemDeTags lista_tarefas =  zip (listaDeTagsExclusivas (listaDeTags lista_tarefas)) (contagemDeTags (listaDeTags lista_tarefas)) -- faz o zip entre a lista de tarefas unicas e a quantidade de vezes que as tarefas aparecem 
  where
  -- retorna a lista total de tags da lista de tarefas(inclui repetidas) de forma recursiva
  listaDeTags [] = [] -- caso base
  listaDeTags (x:xs) = tags x ++ listaDeTags xs -- concatena todas as tags de todas as tarefas

  -- conta quantas vezes uma tag aparece na lista de forma recursiva
  contagemDeTags [] = [] -- caso base
  contagemDeTags (x:xs) = [contagemTagsIguais x xs] ++ contagemDeTags (excluiTagsRepetidas x xs) -- gera uma lista de inteiros com a quantidade de vezes que cada tag aparece  


-- Funcoes de Peristencia dos Dados:

tarefaParaString :: Tarefa -> String
tarefaParaString tarefa = unwords [ show (idTarefa tarefa)
                           , descricao tarefa
                           , show (status tarefa)
                           , show (categoria tarefa)
                           , maybe "Nothing" show (prazo tarefa)
                           , unwords (tags tarefa)
                           ]


salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
salvarEmArquivo arquivo lista_tarefas = writeFile arquivo (unlines (map tarefaParaString lista_tarefas))



-- Daqui pra baixo nao ta funcionando eu acho, tenho q descobrir oq ta errado(provavelmente mta coisa)
lerId :: String -> Maybe Int
lerId id_string = case reads id_string of
                  [(valor, "")] -> Just valor
                  _          -> Nothing


lerStatus :: String -> Maybe Status
lerStatus status_string
 | status_string == "Pendente"  = Just Pendente
 | status_string == "Concluida" = Just Concluida
 | otherwise = Nothing


lerPrioridade :: String -> Maybe Prioridade
lerPrioridade prioridade_string
 | prioridade_string == "Baixa" = Just Baixa
 | prioridade_string == "Media" = Just Media
 | prioridade_string == "Alta"  = Just Alta
 | otherwise= Nothing


lerCategoria :: String -> Maybe Categoria
lerCategoria categoria_string
 | categoria_string == "Trabalho" = Just Trabalho
 | categoria_string == "Estudo"   = Just Estudos
 | categoria_string == "Pessoal"  = Just Pessoal
 | categoria_string == "Outro"    = Just Outro
 | otherwise                      = Nothing


separarString :: Char -> String -> [String]
separarString separador string = case break (== separador) string of
                                 (x, [])     -> [x]
                                 (x, _:xs)   -> x : separarString separador xs

lerDia :: String -> Maybe Day
lerDia dia_string = do
                    let partes = separarString ' ' dia_string
                    if length partes /= 3
                       then Nothing
                       else do
                            let [ano_string, mes_string, dia_string] = partes
                            if not (all isDigit ano_string && all isDigit mes_string && all isDigit dia_string)
                               then Nothing
                               else do
                                    let ano = read ano_string :: Integer
                                        mes = read mes_string :: Int
                                        dia = read dia_string :: Int
                                    if (mes < 1 || mes > 12 || dia < 1 || dia > 31)
                                       then Nothing
                                       else Just (fromGregorian ano mes dia)


stringParaTarefa :: String -> Maybe Tarefa
stringParaTarefa tarefa = do
                          let palavras = words tarefa
                          if length palavras < 6
                             then Nothing
                             else do
                                  let id_tarefa : desc_tarefa : status_tarefa : prioridade_tarefa : categoria_tarefa : prazo_tarefa : tag_tarefa = palavras

                                  id <- lerId id_tarefa
                                  status <- lerStatus status_tarefa
                                  prioridade <- lerPrioridade prioridade_tarefa
                                  categoria <- lerCategoria categoria_tarefa

                                  let prazo = if prazo_tarefa == "Nothing"
                                              then Nothing
                                              else lerDia prazo_tarefa

                                  return (Tarefa
                                          {idTarefa = id
                                          , descricao = desc_tarefa
                                          , status = status
                                          , prioridade = prioridade
                                          , categoria = categoria
                                          , prazo = prazo
                                          , tags = tag_tarefa
                                          })



carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo arquivo = do
                            tarefas <- readFile arquivo
                            let linhas = lines tarefas
                            return [tarefa | Just tarefa <- map stringParaTarefa linhas]
