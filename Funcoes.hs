module Funcoes where

import Tipos

adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]
adicionarTarefa tarefa listaTarefa = tarefa : listaTarefa
-- adicionar depois uma validação para id e um retorno either em caso de erro

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
