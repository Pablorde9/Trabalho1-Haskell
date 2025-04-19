module Funcoes where

import Tipos


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
buscarPorPalavraChave palavra listaTarefas = filter (\t -> contem palavra (descricao t)) listaTarefas
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

--verifica se uma tarefa esta em atraso de forma recursiva, devolvendo apenas tais tarefas
verificarAtrasos :: [Tarefa] -> Day -> [Tarefa]
verificarAtrasos [] _ = []
verificarAtrasos (t:ts) dataAtual
--p recebe o valor prazo da tarfea t e compara com a data fornecida, e comprara se t tambem está pendente
  | Just p <- prazo t, p < dataAtual, status t == Pendente = t : verificarAtrasos ts dataAtual
--caso nao esteja em atraso a lista continua a checar o resto dela, tarefas sem prazo nunca são consideradas em atraso
  | otherwise = verificarAtrasos ts dataAtual

