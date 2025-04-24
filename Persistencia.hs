module Persistencia where

-- importacoes
import Data.Time.Calendar
import Tipos
import System.IO
import Data.List
import Data.Char (isDigit)


-- funcao que converte o tipo tarefa para string
tarefaParaString :: Tarefa -> String
tarefaParaString tarefa = unwords [show (idTarefa tarefa) -- separa em palavras as partes da tarefa. OBS: utiliza '/' como caractere de separacao entre os campos
                           , "/ " ++ descricao tarefa
                           , "/ " ++ show (status tarefa)
                           , "/ " ++ show (prioridade tarefa)
                           , "/ " ++ show (categoria tarefa)
                           , "/ " ++ maybe "Nothing" show (prazo tarefa)
                           , "/ " ++ unwords (tags tarefa)
                           ]


-- recebe um arquivo e uma lista de tarefas, converte as tarefas para string e salva ela no arquivo (uma tarefa por linha)
salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
salvarEmArquivo arquivo lista_tarefas = writeFile arquivo (unlines (map tarefaParaString lista_tarefas))


-- le um string e retorna um int
lerId :: String -> Int
lerId id_string = if isDigit (init id_string) -- checa se o primeiro caractere da string e um numero
                     then (read id_string :: Int) -- se sim, converte para int
                     else error "Formatacao nao identificada" -- se nao, retorna erro


-- le uma string e retorna um tipo Status
lerStatus :: String -> Status
lerStatus status_string
 | status_string == " Pendente "  = Pendente -- se a string estiver nessa formatacao retorna Pendente
 | status_string == " Concluida " = Concluida -- se estiver nessa retorna Concluida
 | otherwise                      = error "Formatacao nao identificada" -- se estiver em alguma diferente retorna erro


-- le uma string e retorna um tipo Prioridade
lerPrioridade :: String -> Prioridade
-- se estiver na formatacao correta retorna o tipo correspondente
lerPrioridade prioridade_string
 | prioridade_string == " Baixa " = Baixa
 | prioridade_string == " Media " = Media
 | prioridade_string == " Alta "  = Alta
 | otherwise                      = error "Formatacao nao identificada" -- se nao retorna erro


-- le uma string e retorna um tipo Categoria
lerCategoria :: String -> Categoria
-- se estiver na formatacao correta retorna o tipo correspondente
lerCategoria categoria_string
 | categoria_string == " Trabalho " = Trabalho
 | categoria_string == " Estudos "   = Estudos
 | categoria_string == " Pessoal "  = Pessoal
 | categoria_string == " Outro "    = Outro
 | otherwise                        = error "Formatacao nao identificada" -- se nao retorna erro


-- recebe um char de controle e uma string, retorna uma lista de strings separadas onde estava o char de controle
separarString :: Char -> String -> [String]
separarString separador string = case break (== separador) string of -- se o elemento for igual ao separador: 
                                 (x, [])     -> [x] -- se for o ultimo elemento, retorna apenas ele em uma lista
                                 (x, _:xs)   -> x : separarString separador xs -- se tiver mais elementos, chama a funcao de forma recursiva


-- converte uma string para o tipo Maybe Day
lerDia :: String -> Maybe Day
lerDia dia_string = let partes = separarString '-' dia_string in -- separa os campos entre ano, mes e dia
                    if length partes /= 3 -- checa se tem os campos corretos
                       then Nothing -- se nao tiver retorna Nothing
                       else let [ano_string, mes_string, dia_string] = partes in -- se tiver coloca em 3 variaveis diferentes
                            -- checa se todos sao ints (tail e init sao usados para corrigir formatacao)
                            if not (all isDigit (tail ano_string) && all isDigit mes_string && all isDigit (init dia_string))
                               then Nothing -- se estiver errado retorna Nothing
                               else let ano = read ano_string :: Integer -- se estiver certo converte para Integer
                                        mes = read mes_string :: Int -- converte para Int
                                        dia = read dia_string :: Int in -- converte para Int
                                    if (mes < 1 || mes > 12 || dia < 1 || dia > 31) -- checa se os dados sao uma data valida
                                       then Nothing -- se nao Nothing
                                       else Just (fromGregorian ano mes dia) -- se sim converte para tipo Maybe Day


-- converte uma string para uma tarefa
stringParaTarefa :: String -> Tarefa
stringParaTarefa tarefa = let partes = separarString '/' tarefa in -- separa a string entre as '/' (caractere de controle usado na funcao de tarefaParaString)
                          let id_tarefa : desc_tarefa : status_tarefa : prioridade_tarefa : categoria_tarefa : prazo_tarefa : tag_tarefa = partes in -- define cada campo da tarefa
                          let prazo = if prazo_tarefa == " Nothing " -- se o dia for Nothing
                                         then Nothing -- retorna Nothing
                                         else lerDia prazo_tarefa -- senao converte para MaybeDay
                         in (Tarefa -- retorna a Tarefa
                            {idTarefa = lerId id_tarefa -- converte de string para Int
                            , descricao = init (tail desc_tarefa) -- formata corretamente a string(elemina espacos)
                            , status = lerStatus status_tarefa -- converte de string para Status
                            , prioridade = lerPrioridade prioridade_tarefa -- converte de string para Prioridade
                            , categoria = lerCategoria categoria_tarefa -- converte de string para Categoria
                            , prazo = prazo -- prazo definido acima
                            , tags = separarString ' ' (tail (head tag_tarefa)) -- formata a string de forma correta e separa em palavras(cada uma e uma tag)
                            })


-- funcao que recebe um arquivo, e retorna uma lista de tarefas a partir das strings escritas nele
carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo arquivo = do
                            tarefas <- readFile arquivo -- guarda o conteudo do arquivo em uma variavel
                            let linhas = lines tarefas -- separa em linhas (cada linha e uma tarefa)
                            return (map stringParaTarefa linhas) -- retorna uma tarefa para cada linha com a funcao stringParaTarefa
