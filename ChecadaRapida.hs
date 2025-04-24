{-# LANGUAGE TemplateHaskell #-}
module ChecadaRapida where

import Test.QuickCheck
import Test.QuickCheck.All
import Data.Time.Calendar (Day, fromGregorian, diffDays)

import Tipos
import Funcoes

-- Instancias para gerar tarefas
instance Arbitrary Status where
  arbitrary = elements [Pendente, Concluida]

instance Arbitrary Prioridade where
  arbitrary = elements [Baixa, Media, Alta]

instance Arbitrary Categoria where
  arbitrary = elements [Trabalho, Estudos, Pessoal, Outro]

instance Arbitrary Day where
  arbitrary = do
    ano  <- choose (2020,2030)
    mes  <- choose (1,12)
    dia  <- choose (1,28)
    return (fromGregorian ano mes dia)

instance Arbitrary Tarefa where
  arbitrary = do
    id_tarefa        <- arbitrary
    descricao_tarefa <- listOf1 (elements ['a'..'z'])
    status_tarefa    <- arbitrary
    prioridade_tarefa<- arbitrary
    categoria_tarefa <- arbitrary
    prazo_tarefa     <- frequency [(3, return Nothing), (1, Just <$> arbitrary)]
    tags_tarefa      <- listOf (listOf1 (elements ['a'..'z']))
    return Tarefa
      { idTarefa   = id_tarefa
      , descricao  = descricao_tarefa
      , status     = status_tarefa
      , prioridade = prioridade_tarefa
      , categoria  = categoria_tarefa
      , prazo      = prazo_tarefa
      , tags       = tags_tarefa
      }

-- Propriedades para cada função em Funcoes.hs
-- O nome das funcoes de teste seguem a estrutura teste_funcaoTestada_focodoTeste 

-- testes do adicionarTarefa
-- testa se adicionarTarefa aumenta a lista
teste_adicionarTarefa_lista :: Tarefa -> [Tarefa] -> Property
teste_adicionarTarefa_lista tarefa lista_tarefa =
    not (idPertenceLista tarefa lista_tarefa) ==>
      length (adicionarTarefa tarefa lista_tarefa) === length lista_tarefa + 1


--testa se adicionarTarefa nao coloca ids duplicados
teste_adicionarTarefa_id :: Tarefa -> [Tarefa] -> Property
teste_adicionarTarefa_id tarefa lista_tarefa =
    idPertenceLista tarefa lista_tarefa ==>
    adicionarTarefa tarefa lista_tarefa === lista_tarefa --caso ja tenha o id na lista, a funcao deveria retornar a lista inalterada

-- testes do removerTarefa
-- testa se removerTarefa reduz a lista em um
teste_removerTarefa_lista :: [Tarefa] -> Property
teste_removerTarefa_lista lista_tarefa =
    not (null lista_tarefa) ==>
      let geraId = elements (todosIds lista_tarefa) --pega um id que pertence a lista
      in forAll geraId (\id_remover -> --forAll pega um gerador de numero e um funcao e vai testando
        length (removerTarefa id_remover lista_tarefa) -- geraId pega um id da lista e o id_remover testa se remover ele da lista reduz em 1
         === length lista_tarefa - 1)

-- testa se removerTarefa em um id inexistente a lista fica inalterada
teste_removerTarefa_id :: [Tarefa] -> Int -> Property
teste_removerTarefa_id lista_tarefa id_invalido =
    id_invalido `notElem` todosIds lista_tarefa ==> --se o id nao pertence a lista tirar ele da lista deixa ela inalterada
    removerTarefa id_invalido lista_tarefa === lista_tarefa

-- marcarConcluida
-- checa se todos os ids da lista se passarem pela funcao ficam concluidos
teste_marcarConcluida_status :: [Tarefa] -> Property
teste_marcarConcluida_status lista_tarefa = 
     not (null lista_tarefa) ==>
       forAll (elements (todosIds lista_tarefa))  --gerador de ids
              (\id_teste ->
       let lista' = marcarConcluida id_teste lista_tarefa
           selecionadas = filter ((== id_teste) . idTarefa) lista' 
        in all ((== Concluida) . status) selecionadas)

-- listarPorCategoria
teste_listarPorCategoria_filtra :: [Tarefa] -> Categoria -> Bool
teste_listarPorCategoria_filtra lista_tarefa categoria_teste =
  all ((== categoria_teste) . categoria) (listarPorCategoria categoria_teste lista_tarefa)

-- ordenarPorPrioridade
-- testa se ordenarPorPrioridade retorna uma lista onde cada tarefa tem prioridade maior ou igual a do proximo
teste_ordenarPorPrioridade_ordem :: [Tarefa] -> Bool
teste_ordenarPorPrioridade_ordem lista_tarefa =
  let ordenada = ordenarPorPrioridade lista_tarefa
      pares = zip ordenada (drop 1 ordenada)
  in all (\(t1, t2) -> prioridade t1 >= prioridade t2) pares --checa se todas sao maiores


-- filtrarPorTag
-- checa se a funcao retorna somente tarefas com a tag de teste
teste_filtrarPorTag_filtra :: [Tarefa] -> String -> Bool
teste_filtrarPorTag_filtra lista_tarefa tag_teste =
  all (elem tag_teste . tags) (filtrarPorTag tag_teste lista_tarefa)

-- calcularDiasRestantes
-- checa se a funcao retorna nothing se nao tiver prazo ou a diferenca dos dias
teste_calcularDiasRestantes_calcula :: Tarefa -> Day -> Bool
teste_calcularDiasRestantes_calcula tarefa data_teste =
  case prazo tarefa of
    Nothing -> calcularDiasRestantes tarefa data_teste == Nothing
    Just dia -> calcularDiasRestantes tarefa data_teste == Just (fromInteger (diffDays dia data_teste))

-- verificarAtrasos
-- checa se a funcao retorna somente tarefas no atraso
teste_verificarAtrasos_verifica :: [Tarefa] -> Day -> Bool
teste_verificarAtrasos_verifica lista_tarefa data_teste =
  all (\t -> case prazo t of
               Just prazo_presente  -> prazo_presente < data_teste && status t == Pendente --pra ser atrasada o prazo e menor e o status pendente
               Nothing -> False)
      (verificarAtrasos lista_tarefa data_teste)

-- quickCheckAll vai checar todas as funcoes teste criadas
checadaRapida :: IO ()
checadaRapida = do
  teste <- $quickCheckAll
  if teste then putStrLn "Parabens! Todos os testes deram certo."
           else putStrLn "Poxa! Alguns testes falharam."
main :: IO ()
main = checadaRapida
