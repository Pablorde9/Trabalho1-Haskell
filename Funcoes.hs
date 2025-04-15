module Funcoes where

import Tipos
-- os nomes das funcoes segue a estrutura pedroColla e de variaveis pedro_colla, pra nao ter confusao

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
