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
