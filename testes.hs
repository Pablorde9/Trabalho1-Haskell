module Testes where

import Funcoes
import Persistencia

lista1 = [ Tarefa 1 "Estudar Haskell" Pendente Alta Estudos
(Just (fromGregorian 2025 04 11)) ["ufu", "haskell"]
, Tarefa 2 "Fazer compras" Conclu´ıda Media Pessoal
(Just (fromGregorian 2025 04 11)) ["casa"]
, Tarefa 3 "Finalizar projeto" Pendente Alta Trabalho
(Just (fromGregorian 2025 04 11)) ["dev", "haskell"]
]

