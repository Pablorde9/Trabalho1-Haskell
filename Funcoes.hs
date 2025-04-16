module Funcoes where

import Tipos
-- os nomes das funcoes segue a estrutura nomeFuncao e de variaveis nome_variavel, pra nao ter confusao

todosIds :: [Tarefa] -> [Int] 
todosIds lista_tarefa = map idTarefa lista_tarefa
-- funcao auxiliar que retorna a lista de todos os ids de uma lista

idPertenceLista :: Tarefa -> [Tarefa] -> Bool
idPertenceLista tarefa lista_tarefa = (elem (idTarefa tarefa) (todosIds lista_tarefa)) 
--checa se o id de uma tarefa ja pertence a uma lista

adicionarTarefa :: Tarefa -> [Tarefa] -> Either String [Tarefa]
adicionarTarefa tarefa lista_tarefa = 
     if (idPertenceLista tarefa lista_tarefa) -- compara se o id da nova tarefa ja pertence a lista
       then Left "Error: ID ja existente" -- se o id da nova tarefa ja existir na lista, retorna mensagem de erro
       else Right (tarefa : lista_tarefa) -- coloca a nova tarefa no comeco da lista de tarefas

removerTarefa :: Int -> [Tarefa] -> [Tarefa]
removerTarefa id = filter (\t -> idTarefa t /= id) 
 -- verifica se existe algum id(t) que seja igual ao id fornecido, e o remove
 -- adicionar um indicador para se a tarefa foi encontrada ou removida

marcarConcluida :: Int -> [Tarefa] -> [Tarefa]
marcarConcluida id = map (\t -> if idTarefa t == id then t { status = Concluida } else t)
-- mapeia por todos elementos ate encontrar o id fornecido, quando encontra muda o status dele para concluido
-- se não encontrar o elemento, nada é alterado
-- adicionar uma forma de verificar se a tarefa realmente exitse 

-- implementar nas funções o Either para casos de erro

--FUNÇÕES AVANÇADAS

listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria categoria_listada = filter(\t -> categoria t == categoria_listada)
-- lista a categoria com filter aplicada junto com a lista fornacida

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade prioridade_listada = filter (\t -> prioridade t == prioridade_listada)
-- filtra por prioridade fornecida

filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
filtrarPorStatus status_listado = filter (\t -> status t == status_listado)
-- filtra pelo status fornecido]

ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
ordenarPorPrioridade [] = []
ordenarPorPrioridade (x:xs) = ordenarPorPrioridade maiores ++ [x] ++ ordenarPorPrioridade menores
   where
      maiores = filter (\t -> prioridade t < prioridade x) (xs)
      --retira os elementos que são menores que x e filtra no resto da lista, deixando apenas os maiores que x
      menores = filter (\t -> prioridade t >= prioridade x) (xs)
      --retira os maiores e iguais a x e filtra no restoda lista, deixando apenas os menores que x 
-- nao testei ainda se ta 100% certo, fica para vcs ai, possivelmente deve ter algum erro

buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
