
import Data.Time.Calendar
data Status = Pendente | Concluida deriving (Show, Eq)
data Prioridade = Baixa | Media | Alta deriving (Show, Eq, Ord)
data Categoria = Trabalho | Estudos | Pessoal | Outro deriving (Show, Eq)
data Tarefa = Tarefa
    { idTarefa :: Int
    , descricao :: String
    , status :: Status
    , prioridade :: Prioridade
    , categoria :: Categoria
    , prazo :: Maybe Day
    , tags :: [String]
    } deriving (Show, Eq)
aed = Tarefa { idTarefa = 123, descricao = "Materia", status = Pendente, prioridade = Baixa, categoria = Estudos, prazo = Nothing, tags = ["oiii"]}

adicionarTarefa :: Tarefa -> [Tarefa] -> Either String [Tarefa]
adicionarTarefa nova_tarefa lista_tarefa =
    if not (null [ t | t <- lista_tarefa, idTarefa t == idTarefa nova_tarefa ])
      then Left "Erro: Ja existe uma tarefa com esse id!"
      else Right (nova_tarefa : lista_tarefa)

todosIds :: [Tarefa] -> [Int]
todosIds lista_tarefa = map idTarefa lista_tarefa

removerTarefa :: Int -> [Tarefa] -> Either [Tarefa] String
removerTarefa id lista_tarefa =
    if (elem id (todosIds lista_tarefa))
      then Left ([t | t <- lista_tarefa, idTarefa t /= id ])
      else Right "Erro: Lista inalterada, nao existe uma tarefa com esse id!"

concluiTarefa :: Int -> [Tarefa] -> [Tarefa]
concluiTarefa id lista_tarefa = map conclui lista_tarefa
    where
      conclui tarefa
        | idTarefa tarefa == id = tarefa {status = Concluida}
        | otherwise = tarefa



marcarConcluida :: Int -> [Tarefa] -> Either [Tarefa] String
marcarConcluida id lista_tarefa = 
    if (elem id (todosIds lista_tarefa))
      then Left (concluiTarefa id lista_tarefa)
      else Right "Erro: Lista inalterada, nao existe uma tarefa com esse id!"

