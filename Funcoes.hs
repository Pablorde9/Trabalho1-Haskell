module Funcoes where

import Tipos

adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]
adicionarTarefa tarefa lista = tarefa : lista --adicionar depois uma validação para id

removerTarefa :: Int -> [Tarefa] -> [Tarefa]

marcarConcluida :: Int -> [Tarefa] -> [Tarefa]

