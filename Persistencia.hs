module Persistencia where

import Data.Time.Calendar
import Tipos
import System.IO
import Data.List
import Data.Char (isDigit)

tarefaParaString :: Tarefa -> String
tarefaParaString tarefa = unwords [show (idTarefa tarefa)
                           , "/ " ++ descricao tarefa
                           , "/ " ++ show (status tarefa)
                           , "/ " ++ show (prioridade tarefa)
                           , "/ " ++ show (categoria tarefa)
                           , "/ " ++ maybe "Nothing" show (prazo tarefa)
                           , "/ " ++ unwords (tags tarefa)
                           ]

salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
salvarEmArquivo arquivo lista_tarefas = writeFile arquivo (unlines (map tarefaParaString lista_tarefas))



-- Daqui pra baixo nao ta funcionando eu acho, tenho q descobrir oq ta errado(provavelmente mta coisa)
lerId :: String -> Int
lerId id_string = if isDigit (head id_string)
                     then (read id_string :: Int)
                     else -1


lerStatus :: String -> Status
lerStatus status_string
 | status_string == " Pendente "  = Pendente
 | status_string == " Concluida " = Concluida
 | otherwise                      = error "Formatacao nao identificada"


lerPrioridade :: String -> Prioridade
lerPrioridade prioridade_string
 | prioridade_string == " Baixa " = Baixa
 | prioridade_string == " Media " = Media
 | prioridade_string == " Alta "  = Alta
 | otherwise                      = error "Formatacao nao identificada"


lerCategoria :: String -> Categoria
lerCategoria categoria_string
 | categoria_string == " Trabalho " = Trabalho
 | categoria_string == " Estudos "   = Estudos
 | categoria_string == " Pessoal "  = Pessoal
 | categoria_string == " Outro "    = Outro
 | otherwise                        = error "Formatacao nao identificada"


separarString :: Char -> String -> [String]
separarString separador string = case break (== separador) string of
                                 (x, [])     -> [x]
                                 (x, _:xs)   -> x : separarString separador xs

lerDia :: String -> Maybe Day
lerDia dia_string = let partes = separarString '-' dia_string in
                    if length partes /= 3
                       then Nothing
                       else let [ano_string, mes_string, dia_string] = partes in
                            if not (all isDigit (tail ano_string) && all isDigit mes_string && all isDigit (init dia_string))
                               then Nothing
                               else let ano = read ano_string :: Integer
                                        mes = read mes_string :: Int
                                        dia = read dia_string :: Int in
                                    if (mes < 1 || mes > 12 || dia < 1 || dia > 31)
                                       then Nothing
                                       else Just (fromGregorian ano mes dia)


stringParaTarefa :: String -> Tarefa
stringParaTarefa tarefa = let partes = separarString '/' tarefa in
                          let id_tarefa : desc_tarefa : status_tarefa : prioridade_tarefa : categoria_tarefa : prazo_tarefa : tag_tarefa = partes in
                          let prazo = if prazo_tarefa == " Nothing "
                                         then Nothing
                                         else lerDia prazo_tarefa
                         in (Tarefa
                            {idTarefa = lerId id_tarefa
                            , descricao = init (tail desc_tarefa)
                            , status = lerStatus status_tarefa
                            , prioridade = lerPrioridade prioridade_tarefa
                            , categoria = lerCategoria categoria_tarefa
                            , prazo = prazo
                            , tags = separarString ' ' (tail (head tag_tarefa))
                            })



carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo arquivo = do
                            tarefas <- readFile arquivo
                            let linhas = lines tarefas
                            return (map stringParaTarefa linhas)
