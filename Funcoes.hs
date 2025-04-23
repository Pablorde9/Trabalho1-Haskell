module Funcoes where

import Tipos
import Data.Time (Day, parseTimeM, defaultTimeLocale)
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


-- Imprime uma mensagem dizendo se a adicao de Tarefa na lista foi bem sucedida ou nao
validarAdicao :: Tarefa -> [Tarefa] -> IO ()
validarAdicao tarefa lista_tarefa = do
                if idPertenceLista tarefa lista_tarefa -- checa se ja existe um id na lista igual ao da tarefa tentando ser adicionada
                   then putStrLn "Erro! esse id ja foi cadastrado por outra tarefa, lista inalterada." -- caso sim
                   else putStrLn "Lista atualizada com sucesso!" -- caso nao

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
--calcularDiasRestantes :: Tarefa -> Data -> Maybe Int
--calcularDiasRestantes tarefa dataAtual =
--  case prazo tarefa of
--    Nothing -> Nothing --tarefa sem prazo definido
--    Just p -> Just (diferencaDias p dataAtual) --calcula dias restantes

-- Função auxiliar para calcular diferença entre datas
--diferencaDias :: Data -> Data -> Int
--diferencaDias (Data a1 m1 d1) (Data a2 m2 d2) =
--  (a1 - a2) * 360 + (m1 - m2) * 30 + (d1 - d2) --aproximação de mês para 30 dias




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

-- funcao auxiliar para calcular quantas tarefas ha na lista
qtdTarefas :: [Tarefa] -> Int
qtdTarefas []     = 0
qtdTarefas (_:xs) = 1 + qtdTarefas xs

--funcao auxiliar para calcular a porcentagem de tarefas com determinado tipo
porcentagemTarefa :: [Tarefa] -> Int -> Double
porcentagemTarefa lista_tarefa qtdx = 100 * fromIntegral qtdx / fromIntegral (qtdTarefas lista_tarefa)

-- **ainda falta um pouco de testes - se printa bonitinho no main
-- funcao que retorna um resumo das tarefas dentro da lista
relatorioTarefa :: [Tarefa] -> String
relatorioTarefa lista_tarefa =
    let total = qtdTarefas lista_tarefa
        pendente = length (filter ((==Pendente) . status) lista_tarefa)
        concluida = length (filter ((==Concluida) . status) lista_tarefa)
        qtdTrab = length (filter ((==Trabalho) . categoria) lista_tarefa)
        qtdEst = length (filter ((==Estudos) . categoria) lista_tarefa)
        qtdPes = length (filter ((==Pessoal) . categoria) lista_tarefa)
        qtdOut = length (filter ((==Outro) . categoria) lista_tarefa)
    in unlines -- unlines junta strings e adicionar o \n
       [ "Relatorio Resumido:"
       , "- Total de tarefas: " ++ show total
       , "- Pendentes: " ++ show pendente ++ " | Concluidas: " ++ show concluida
       , "- Distribuicao por categoria:"
       , "* Trabalho: " ++ show qtdTrab ++ " tarefas (" ++ show (porcentagemTarefa lista_tarefa qtdTrab) ++ "%)"
       , "* Estudos: " ++ show qtdEst ++ " tarefas (" ++ show (porcentagemTarefa lista_tarefa qtdEst) ++ "%)"
       , "* Pessoal: " ++ show qtdPes ++ " tarefas (" ++ show (porcentagemTarefa lista_tarefa qtdPes) ++ "%)"
       , "* Outro: " ++ show qtdOut ++ " tarefas (" ++ show (porcentagemTarefa lista_tarefa qtdOut) ++ "%)"
       ]

--Funçoes do Display
menu :: [Tarefa] -> IO ()
menu lista_tarefas = do
                       putStrLn "Bem vindo ao Sistema de Gerenciamento de Tarefas!"
                       putStrLn "Digite o que deseja fazer:"
                       putStrLn "1. Modificar a Lista de Tarefas."
                       putStrLn "2. Verificar alguma Informação na lista."
                       putStrLn "3. Gestão de Prazos."
                       putStrLn "4. Carregar Uma lista."
                       putStrLn "5. Salvar."
                       putStrLn "6. Sair."
                       opcao <- readLn
                       case opcao of
                         1 -> do
                                putStrLn "O que deseja fazer com a lista?"
                                putStrLn "1. Adicionar tarefa."
                                putStrLn "2. Remover tarefa."
                                putStrLn "3. Marcar uma tarefa como concluída"
                                opcao10 <- readLn
                                case opcao10 of
                                  1 -> do
                                         novalista <- adicionarTarefaIO lista_tarefas
                                         menu novalista
                                  2 -> putStrLn "removerTarefa"
                                  3 -> putStrLn "marcarCocluida"
                         2 -> do
                                putStrLn "O que deseja saber sobre a Lista?"
                                putStrLn "1. Listar tarefas de uma categoria específica."
                                putStrLn "2. Listar tarefas com uma prioridade específica"
                                putStrLn "3. Listar tarefas cuja descrição contenha uma palavra-chave"
                                putStrLn "4. Listar tarefas com um status específico."
                                putStrLn "5. Ordernar por prioridade(da mais alta para mais baixa)."
                                putStrLn "6. Listar tarefas que contenham uma tag específica."
                                putStrLn "7. Listar as tags e suas frêquencias de uso."
                                opcao20 <- readLn
                                case opcao20 of
                                  1 -> putStrLn "listarPorCategoria"
                                  2 -> putStrLn "listarPorPrioridade"
                                  3 -> putStrLn "buscarPorPalavraChave"
                                  4 -> putStrLn "filtrarPorStatus"
                                  5 -> putStrLn "ordenarPorPrioridade"
                                  6 -> putStrLn "filtrarPorTag"
                                  7 -> putStrLn "nuvemDeTags"
                         3 -> do
                                putStrLn "Ferramentas de gestão:"
                                putStrLn "1. Verificar tarefas atrasadas."
                                putStrLn "2. Tempo restante para realizar uma tarefa."
                                opcao30 <- readLn
                                case opcao30 of
                                  1 -> putStrLn "verificarAtrasos"
                                  2 -> putStrLn "calcularDiasRestantes"
                         4 -> do
                                putStrLn "salvarEmArquivo"
                         5 -> do
                                putStrLn "carregarDeArquivo"
                         6 -> do
                                putStrLn"Até mais!"

                         _ -> do
                                putStrLn "Opção Inválida!"

-- Função auxiliar para converter String -> Maybe Day
convertePrazo :: String -> Maybe Day
convertePrazo "" = Nothing
convertePrazo s = parseTimeM True defaultTimeLocale "%Y-%m-%d" s

--Função AdicionarTarefa com saida em IO
adicionarTarefaIO :: [Tarefa] -> IO [Tarefa]
adicionarTarefaIO lista_tarefa = do
                                   putStrLn "Digite a descrição da tarefa: "
                                   descricao <- getLine
                                   putStrLn "Digite o status(Pendente | Concluida): "
                                   status_str <- getLine
                                   let status = read status_str :: Status
                                   putStrLn "Digite a prioridade(Baixa | Media | Alta): "
                                   prioridade_str <- getLine
                                   let prioridade = read prioridade_str :: Prioridade
                                   putStrLn "Digite a categoria(Trabalho | Estudos | Pessoal | Outro): "
                                   categoria_str <- getLine
                                   let categoria = read categoria_str :: Categoria
                                   putStrLn "Digite o prazo (formato: YYYY-MM-DD): "
                                   prazo_str <- getLine
                                   let prazo = convertePrazo prazo_str
                                   putStrLn "Digite as tags separadas por espaço: "
                                   tags_str <- getLine
                                   let tags = words tags_str
                                   let tarefa = Tarefa ((qtdTarefas lista_tarefa) + 1) descricao status prioridade categoria prazo tags
                                   let novaLista = adicionarTarefa tarefa lista_tarefa
                                   putStrLn "Tarefa adicionada com sucesso!"
                                   return novaLista

