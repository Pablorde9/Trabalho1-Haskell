module Persistencia where

import Tipos
import System.IO
import Data.List

salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
salvarEmArquivo arquivo lista_tarefas = writeFile arquivo (unlines (map tarefaParaString lista_tarefas))



-- Daqui pra baixo nao ta funcionando eu acho, tenho q descobrir oq ta errado(provavelmente mta coisa)
lerId :: String -> Maybe Int
lerId id_string = case reads id_string of
                  [(valor, "")] -> Just valor
                  _          -> Nothing


lerStatus :: String -> Maybe Status
lerStatus status_string
 | status_string == "Pendente"  = Just Pendente
 | status_string == "Concluida" = Just Concluida
 | otherwise = Nothing


lerPrioridade :: String -> Maybe Prioridade
lerPrioridade prioridade_string
 | prioridade_string == "Baixa" = Just Baixa
 | prioridade_string == "Media" = Just Media
 | prioridade_string == "Alta"  = Just Alta
 | otherwise= Nothing


lerCategoria :: String -> Maybe Categoria
lerCategoria categoria_string
 | categoria_string == "Trabalho" = Just Trabalho
 | categoria_string == "Estudo"   = Just Estudos
 | categoria_string == "Pessoal"  = Just Pessoal
 | categoria_string == "Outro"    = Just Outro
 | otherwise                      = Nothing


separarString :: Char -> String -> [String]
separarString separador string = case break (== separador) string of
                                 (x, [])     -> [x]
                                 (x, _:xs)   -> x : separarString separador xs

lerDia :: String -> Maybe Day
lerDia dia_string = do
                    let partes = separarString ' ' dia_string
                    if length partes /= 3
                       then Nothing
                       else do
                            let [ano_string, mes_string, dia_string] = partes
                            if not (all isDigit ano_string && all isDigit mes_string && all isDigit dia_string)
                               then Nothing
                               else do
                                    let ano = read ano_string :: Integer
                                        mes = read mes_string :: Int
                                        dia = read dia_string :: Int
                                    if (mes < 1 || mes > 12 || dia < 1 || dia > 31)
                                       then Nothing
                                       else Just (fromGregorian ano mes dia)


stringParaTarefa :: String -> Maybe Tarefa
stringParaTarefa tarefa = do
                          let palavras = words tarefa
                          if length palavras < 6
                             then Nothing
                             else do
                                  let id_tarefa : desc_tarefa : status_tarefa : prioridade_tarefa : categoria_tarefa : prazo_tarefa : tag_tarefa = palavras

                                  id <- lerId id_tarefa
                                  status <- lerStatus status_tarefa
                                  prioridade <- lerPrioridade prioridade_tarefa
                                  categoria <- lerCategoria categoria_tarefa

                                  let prazo = if prazo_tarefa == "Nothing"
                                              then Nothing
                                              else lerDia prazo_tarefa

                                  return (Tarefa
                                          {idTarefa = id
                                          , descricao = desc_tarefa
                                          , status = status
                                          , prioridade = prioridade
                                          , categoria = categoria
                                          , prazo = prazo
                                          , tags = tag_tarefa
                                          })



carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo arquivo = do
                            tarefas <- readFile arquivo
                            let linhas = lines tarefas
                            return [tarefa | Just tarefa <- map stringParaTarefa linhas]
