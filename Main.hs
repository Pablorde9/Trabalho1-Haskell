module Main (main) where

import Funcoes
import System.Exit (exitSuccess)

main :: IO ()
main = do
          putStrLn "Bem vindo ao Sistema de Gerenciamento de Tarefas!"
          putStrLn "Digite o que deseja fazer:"
          putStrLn "1. Modificar a Lista de Tarefas."
          putStrLn "2. Verificar alguma Informação na lista."
          putStrLn "3. Gestão de Prazos."
          putStrLn "4. Sair."
          opcao <- readLn
          case opcao of
            1 -> do
                   putStrLn "O que deseja fazer com a lista?"
                   putStrLn "1. Adicionar tarefa."
                   putStrLn "2. Remover tarefa."
                   putStrLn "3. Marcar uma tarefa como concluída"
                   opcao10 <- readLn
                   case opcao10 of
                     1 -> putStrLn "adicionarTarefa"
                     2 -> putStrLn "removerTarefa"
                     3 -> putStrLn "marcarCocluida"
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
                     1 -> putStrLn "listarPorCategoria"
                     2 -> putStrLn "listarPorPrioridade"
                     3 -> putStrLn "buscarPorPalavraChave"
                     4 -> putStrLn "filtrarPorStatus"
                     5 -> putStrLn "ordenarPorPrioridade"
                     6 -> putStrLn "filtrarPorTag"
                     7 -> putStrLn "nuvemDeTags"
            3 -> do
                   putStrLn "Ferramentas de gestão:"
                   putStrLn "1. Verificar tarefas atrasadas."
                   putStrLn "2. Tempo restante para realizar uma tarefa."
                   opcao30 <- readLn
                   case opcao30 of
                     1 -> putStrLn "verificarAtrasos"
                     2 -> putStrLn "calcularDiasRestantes"
            4 -> do
                   putStrLn"Até mais!"
                   exitSuccess

            _ -> do
                   putStrLn "Opção Inválida!"
                   main
