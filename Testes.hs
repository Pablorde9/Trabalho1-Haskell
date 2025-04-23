module Testes where

import Funcoes
import Persistencia
import Tipos
import Data.Time.Calendar

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

-- funcao que aplica as funcoes basicas implementadas as tarefas(tarefa1, tarefa2, tarefa3) pre definidas
testeFuncoesBasicas :: IO ()
testeFuncoesBasicas = do
                      putStrLn "A partir da tarefa:"
                      print tarefa1
                      putStrLn ""
                      putStrLn "adicionaremos nosso primeiro elemento em uma lista!"
                      let lista_um_elem = adicionarTarefa tarefa1 []
                      validarAdicao tarefa1 []
                      putStrLn ""
                      putStrLn "Nova lista:"
                      putStrLn (unlines (map show lista_um_elem))
                      putStrLn ""
                      putStrLn "Agora adicionaremos tambem os elementos:"
                      print tarefa2
                      let lista_dois_elem = adicionarTarefa tarefa2 lista_um_elem
                      validarAdicao tarefa2 lista_um_elem
                      putStrLn ""
                      print tarefa3
                      let lista_tres_elem = adicionarTarefa tarefa3 lista_dois_elem
                      validarAdicao tarefa3 lista_dois_elem
                      putStrLn ""
                      putStrLn "E agora temos nossa lista completa!"
                      putStrLn (unlines (map show lista_tres_elem))
                      putStrLn ""
                      putStrLn "E podemos remover elementos tambem, vamos atualizar nossa lista removendo a tarefa de id: 2!"
                      let lista_elem_um_tres = removerTarefa 2 lista_tres_elem
                      putStrLn (unlines (map show lista_elem_um_tres))
                      putStrLn ""
                      putStrLn "Tambem podemos marcar uma tarefa pendente como conluida"
                      putStrLn "dada nossa lista com tres tarefas:"
                      putStrLn (unlines (map show lista_tres_elem))
                      putStrLn "vamos marcar a tarefa de id:3 como concluida!"
                      putStrLn ""
                      let lista_elem_tres_concluido = marcarConcluida 3 lista_tres_elem
                      putStrLn "E agora nossa lista esta atualizada:"
                      putStrLn (unlines (map show lista_elem_tres_concluido))
                      putStrLn ""
                      putStrLn "Por fim, podemos checar que a lista nao adiciona uma tarefa se o mesmo id ja estiver cadastrado nela"
                      putStrLn "Lista antes da tentativa:"
                      putStrLn (unlines (map show lista_elem_tres_concluido))
                      putStrLn ""
                      let lista_elem_repetido = adicionarTarefa tarefa1 lista_elem_tres_concluido
                      validarAdicao tarefa1 lista_elem_tres_concluido
                      putStrLn ""
                      putStrLn "E agora podemos ver que a lista continua inalterada"
                      putStrLn (unlines (map show lista_elem_repetido))


-- funcao que aplica as funcoes avancadas implementadas a lista(lista1) pre definida
testeFuncoesAvancadas :: IO ()
testeFuncoesAvancadas = do
                        putStrLn "Dada a lista:"
                        putStrLn (unlines (map show lista1))
                        putStrLn ""
                        putStrLn "organizaremos ela por prioridade:"
                        let lista1_prioridade = ordenarPorPrioridade lista1
                        putStrLn (unlines (map show lista1_prioridade))
                        putStrLn ""
                        putStrLn "E agora filtraremos apenas as tarefas pendentes:"
                        let lista1_pendentes = filtrarPorStatus Pendente lista1_prioridade
                        putStrLn (unlines (map show lista1_pendentes))
                        putStrLn ""
                        putStrLn "Agora vamos ver apenas as tarefas de prioridade alta pendentes:"
                        let lista1_pendentes_alta = listarPorPrioridade Alta lista1_pendentes
                        putStrLn (unlines (map show lista1_pendentes_alta))
                        putStrLn ""
                        putStrLn "Por fim, mostraremos apenas as tarefas pendentes de prioridade alta da categoria pessoal:"
                        let lista1_pendentes_alta_pessoal = listarPorCategoria Pessoal lista1_pendentes_alta
                        putStrLn (unlines (map show lista1_pendentes_alta_pessoal))
                        putStrLn "Que nos retorna uma lista vazia"
                        putStrLn ""
                        putStrLn "Ou podemos procurar por uma palavra chave especifica na descricao de alguma tarefa, como 'Haskell':"
                        let lista1_desc_haskell = buscarPorPalavraChave "Haskell" lista1
                        putStrLn (unlines (map show lista1_desc_haskell))

-- funcao auxiliar para converte o tipo maybe Int para string, para o teste de gestao de prazo
maybeParaString :: Maybe Int -> String
maybeParaString (Just n) = show n -- se for um numero, mostra string com o numero
maybeParaString Nothing = "Nothing" -- senao, string com "Nothing"

-- funcao que aplica as funcoes de gestao de prazo implementadas a lista(lista2) e tarefa(tarefa4) pre definidas
testeFuncoesDeGestaoDePrazo ::Day -> IO ()
testeFuncoesDeGestaoDePrazo dia_atual = do
                                        putStrLn "Dada a lista: "
                                        putStrLn (unlines (map show lista2))
                                        putStrLn ""
                                        putStrLn "Podemos checar quais tarefas estao atrasadas!"
                                        let lista2_atrasada = verificarAtrasos lista2 dia_atual
                                        putStrLn (unlines (map show lista2_atrasada))
                                        putStrLn ""
                                        putStrLn "Ou verificar os dias restantes de uma tarefa! Entao para a tarefa:"
                                        print tarefa1
                                        let tarefa1_tempo_restante = calcularDiasRestantes tarefa1 dia_atual
                                        let tarefa1_string = maybeParaString tarefa1_tempo_restante
                                        if tarefa1_string == "Nothing"
                                           then putStr "Essa Tarefa nao tem prazo!"
                                           else putStr ("Faltam: " ++ tarefa1_string ++ " dias")
                                        putStrLn ""
                                        putStrLn ""
                                        putStrLn "Mas para a tarefa:"
                                        print tarefa4
                                        let tarefa4_tempo_restante = calcularDiasRestantes tarefa4 dia_atual
                                        let tarefa4_string = maybeParaString tarefa4_tempo_restante
                                        if tarefa4_string == "Nothing"
                                           then putStr "Essa Tarefa nao tem prazo!"
                                           else putStr ("Faltam: " ++ tarefa4_string ++ " dias")
                                        putStrLn ""

-- funcao que aplica as funcoes de tags implementadas a lista(lista2) pre definida
testeFuncoesDeTags :: IO ()
testeFuncoesDeTags = do
                     putStrLn "Dada a lista:"
                     putStrLn (unlines (map show lista2))
                     putStrLn ""
                     putStrLn "Filtraremos todas as tarefas com a tag 'hobby':"
                     let lista2_hobby = filtrarPorTag "hobby" lista2
                     putStrLn (unlines (map show lista2_hobby))
                     putStrLn ""
                     putStrLn "Ou podemos gerar uma lista com a frequencia de uso de todas as tags da lista:"
                     let lista2_frequencia_tags = nuvemDeTags lista2
                     putStrLn (unlines (map show lista2_frequencia_tags))


-- funcao que aplica as funcoes de persistencia de dados implementadas a lista(lista1) pre definida
testeFuncoesDePersistenciaDeDados :: IO ()
testeFuncoesDePersistenciaDeDados = do
                           putStrLn "Para que os dados nao sejam perdidos quando encerrarmos o programa, podemos guardar nossa lista em um arquivo!"
                           putStrLn "Para este exemplo usaremos a lista:"
                           putStrLn (unlines (map show lista1))
                           putStrLn ""
                           putStrLn "Certifique-se que existe no mesmo diretorio um arquivo 'arqteste.txt', caso nao exista crie com 'touch arqteste.txt' no terminal!"
                           putStrLn "E agora guardemos seus dados no arquivo 'arqteste.txt'"
                           salvarEmArquivo "arqteste.txt" lista1
                           putStrLn "Pronto! para checar os dados use o comando 'cat arqteste.txt' no seu terminal"
                           putStrLn ""
                           putStrLn "E agora vamos carregar nossa lista de volta do arquivo!"
                           tarefas_carregadas <- carregarDeArquivo "arqteste.txt"
                           putStrLn (unlines (map show tarefas_carregadas))


-- funcao que aplica a funcao de relatorio implementada a lista(lista2) pre definida
testeRelatorio :: IO ()
testeRelatorio  = do
                  putStrLn "Geraremos um pequeno relatorio da lista:"
                  putStrLn (unlines (map show lista2))
                  putStrLn ""
                  relatorioTarefa lista2
