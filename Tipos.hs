module Tipos where

import Data.Time.Calendar

data Status = Pendente | Concluida deriving (Show, Eq, Read)

data Prioridade = Baixa | Media | Alta deriving (Show, Eq, Ord, Read)

data Categoria = Trabalho | Estudos | Pessoal | Outro deriving (Show, Eq, Read, Enum, Bounded)

data Tarefa = Tarefa
    { idTarefa :: Int
    , descricao :: String
    , status :: Status
    , prioridade :: Prioridade
    , categoria :: Categoria
    , prazo :: Maybe Day
    , tags :: [String]
    } deriving (Show, Eq)
    


-- tarefas pre definidas para uso das funcoes de testes
tarefa1 = Tarefa 1 "Estudar Haskell" Pendente Media Estudos (Just (fromGregorian 2025 05 11)) ["ufu", "haskell"]
tarefa2 = Tarefa 2 "Fazer compras" Concluida Baixa Pessoal (Just (fromGregorian 2025 04 11)) ["casa"]
tarefa3 = Tarefa 3 "Finalizar projeto" Pendente Alta Trabalho (Just (fromGregorian 2025 04 11)) ["dev", "haskell"]
tarefa4 = Tarefa 4 "Ler Dom Casmurro" Pendente Baixa Pessoal Nothing ["leitura", "hobby", "Dom Casmurro"]

-- listas pre definidas para uso das funcoes de testes
lista1 = [tarefa3, tarefa2, tarefa1]

lista2 = [ Tarefa 1 "Estudar Ingles" Pendente Media Estudos (Just (fromGregorian 2025 04 21)) ["idiomas", "ingles", "hobby"]
         , Tarefa 2 "Entregar Benchmark" Concluida  Alta Trabalho (Just (fromGregorian 2025 04 21)) ["cuda", "benchmark", "dev"]
         , Tarefa 3 "Ler Dom Casmurro" Pendente Baixa Pessoal (Just (fromGregorian 2025 05 02)) ["leitura", "hobby", "Dom Casmurro"]
         ]
