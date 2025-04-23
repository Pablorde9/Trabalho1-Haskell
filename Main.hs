module Main where

import Tipos
import Funcoes
import Persistencia
import Testes
import System.IO
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import System.Directory (doesFileExist)
import Control.Monad (when)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)

--Funçoes do Display
menu :: [Tarefa] -> IO ()
menu lista_tarefas = do
                       putStrLn "Bem vindo ao Sistema de Gerenciamento de Tarefas!"
                       putStrLn "Digite o que deseja fazer:"
                       putStrLn "1. Modificar a Lista de Tarefas."
                       putStrLn "2. Verificar alguma Informação na lista."
                       putStrLn "3. Mostrar Lista"
                       putStrLn "4. Gestão de Prazos."
                       putStrLn "5. Relatorio"
                       putStrLn "6. Carregar Uma lista."
                       putStrLn "7. Salvar."
                       putStrLn "8. Sair."
                       opcao <- readLn
                       case opcao of
                         1 -> do
                                putStrLn "O que deseja fazer com a lista?"
                                putStrLn "1. Adicionar tarefa."
                                putStrLn "2. Remover tarefa."
                                putStrLn "3. Marcar uma tarefa como concluída"
                                opcao10 <- readLn
                                case opcao10 of
                                  1 -> do
                                         novalista <- adicionarTarefaIO lista_tarefas
                                         menu novalista
                                  2 -> do
                                         novalista <- removerTarefaIO lista_tarefas
                                         menu novalista
                                  3 -> do
                                         novalista <- marcarConcluidaIO lista_tarefas
                                         menu novalista
                         2 -> do
                                putStrLn "O que deseja saber sobre a Lista?"
                                putStrLn "1. Listar tarefas de uma categoria específica."
                                putStrLn "2. Listar tarefas com uma prioridade específica"
                                putStrLn "3. Listar tarefas cuja descrição contenha uma palavra-chave"
                                putStrLn "4. Listar tarefas com um status específico."
                                putStrLn "5. Ordernar por prioridade(da mais alta para mais baixa)."
                                putStrLn "6. Listar tarefas que contenham uma tag específica."
                                putStrLn "7. Listar as tags e suas frêquencias de uso."
                                opcao20 <- readLn
                                case opcao20 of
                                  1 -> do
                                         putStrLn "Digite a categoria que será listada: "
                                         categoria_Str <- getLine
                                         let categoria = read categoria_Str :: Categoria
                                         let novaLista = listarPorCategoria categoria lista_tarefas
                                         print novaLista
                                         menu lista_tarefas
                                  2 -> do
                                         putStrLn "Digite a prioridade que será listada: "
                                         prioridade_Str <- getLine
                                         let prioridade = read prioridade_Str :: Prioridade
                                         let novaLista = listarPorPrioridade prioridade lista_tarefas
                                         print novaLista
                                         menu lista_tarefas
                                  3 -> do
                                         putStrLn "Digite a palavra-chave que será listada: "
                                         palavra_Str <- getLine
                                         let novaLista = buscarPorPalavraChave palavra_Str lista_tarefas
                                         print novaLista
                                         menu lista_tarefas
                                  4 -> do
                                         putStrLn "Digite a status que será listado: "
                                         status_Str <- getLine
                                         let status = read status_Str :: Status
                                         let novaLista = filtrarPorStatus status lista_tarefas
                                         print novaLista
                                         menu lista_tarefas
                                  5 -> do
                                         let novaLista = ordenarPorPrioridade lista_tarefas
                                         print novaLista
                                         menu lista_tarefas
                                  6 -> do
                                         putStrLn "Digite a tag que será listada: "
                                         tag_Str <- getLine
                                         let novaLista = filtrarPorTag tag_Str lista_tarefas
                                         print novaLista
                                         menu lista_tarefas
                                  7 -> do
                                         let novaLista = nuvemDeTags lista_tarefas
                                         print novaLista
                                         menu lista_tarefas
                         3 -> do
                                mostrarLista lista_tarefas
                                menu lista_tarefas
                         4 -> do
                                putStrLn "Ferramentas de gestão:"
                                putStrLn "1. Verificar tarefas atrasadas."
                                putStrLn "2. Tempo restante para realizar uma tarefa."
                                opcao30 <- readLn
                                case opcao30 of
                                  1 -> do
                                         --let novaLista = verificarAtrasos lista_tarefas
                                        -- print novaLista
                                         menu lista_tarefas
                                  2 -> do
                                         putStrLn "calcularDiasRestantes"
                                         menu lista_tarefas
                         5 -> do
                                relatorioTarefa lista_tarefas
                                menu lista_tarefas
                         6 -> do
                                putStrLn "Digite onde você deseja salvar o arquivo: "
                                arquivo_Str <- getLine
                                salvarEmArquivo arquivo_Str lista_tarefas
                                putStrLn "Lista salva com sucesso!"
                                menu lista_tarefas
                         7 -> do
                                putStrLn "Digite onde o caminho do arquivo que você deseja carregar: "
                                arquivo_Str <- getLine
                                lista <- carregarDeArquivo arquivo_Str
                                let novalista = lista
                                menu novalista
                         8 -> do
                                putStrLn"Até mais!"

                         _ -> do
                                putStrLn "Opção Inválida!"

-- Função auxiliar para converter String -> Maybe Day
convertePrazo :: String -> Maybe Day
convertePrazo "" = Nothing -- se a string for vazia não tem o que converter
convertePrazo s = parseTimeM True defaultTimeLocale "%Y-%m-%d" s -- faz a conversão para o tipo day

--Função AdicionarTarefa com saida em IO
adicionarTarefaIO :: [Tarefa] -> IO [Tarefa]
adicionarTarefaIO lista_tarefa = do
                                   putStrLn "Digite o id da tarefa: "
                                   id <- readLn
                                   putStrLn "Digite a descrição da tarefa: "
                                   descricao <- getLine
                                   putStrLn "Digite o status(Pendente | Concluida): "
                                   status_str <- getLine -- recebe o status como uma string
                                   let status = read status_str :: Status -- converte a string para o tipo Status
                                   putStrLn "Digite a prioridade(Baixa | Media | Alta): "
                                   prioridade_str <- getLine -- recebe a prioridade como uma string
                                   let prioridade = read prioridade_str :: Prioridade -- coverte a string para o tipo Prioridade
                                   putStrLn "Digite a categoria(Trabalho | Estudos | Pessoal | Outro): "
                                   categoria_str <- getLine -- recebe a categoria como um string
                                   let categoria = read categoria_str :: Categoria -- converte a string para o tipo Categoria
                                   putStrLn "Digite o prazo (formato: YYYY-MM-DD): "
                                   prazo_str <- getLine -- recebe o prazo como uma string
                                   let prazo = convertePrazo prazo_str -- utiliza a função auxiliar para converte a string no tipo Day(YYYY-MM-DD)
                                   putStrLn "Digite as tags separadas por espaço: "
                                   tags_str <- getLine -- recebe as tagas em forma de string
                                   let tags = words tags_str -- separa as tags e cria um lista com elas
                                   let tarefa = Tarefa id descricao status prioridade categoria prazo tags -- cria a nova tarefa
                                   let novaLista = adicionarTarefa tarefa lista_tarefa 
                                   validarAdicao tarefa lista_tarefa -- verifica se o id fornecido ja estava cadastrado na lista
                                   return novaLista

--Função RemoverTarefa com saida em IO
removerTarefaIO :: [Tarefa] -> IO [Tarefa]
removerTarefaIO lista_tarefas = do
                                 putStrLn "Digite o id da tarefa que você deseja remover: "
                                 id <- readLn
                                 let novaLista = removerTarefa id lista_tarefas
                                 putStrLn "Tarefa removida com sucesso!"
                                 return novaLista

--Função que printa a lista na forma (id, descrição)
mostrarLista :: [Tarefa] -> IO ()
mostrarLista [] = putStrLn "Lista Vazia."
mostrarLista lista_tarefas = do
                              let novaLista = zip (map idTarefa lista_tarefas) (map descricao lista_tarefas) -- junta duas lista na forma de uma tupla, a primeira formadas pelos ids e a segunda pelas descricoes
                              print novaLista


--Função marcarConcluida com saida em IO
marcarConcluidaIO :: [Tarefa] -> IO [Tarefa]
marcarConcluidaIO lista_tarefas = do
                                   putStrLn "Digite o id da tarefa que vôce deseja atualizar: "
                                   id <- readLn
                                   let novaLista = marcarConcluida id lista_tarefas
                                   putStrLn "Tarefa atualizada com sucesso!"
                                   return novaLista



main :: IO ()
main = do 
         menu []
