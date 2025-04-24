-> Primeiro Trabalho
feito por: Gabriel Valentin, Pablo Rodrigues, Pedro de Colla, Ruan Pablo

-> Descrição
Sistema de gerenciamento de tarefas desenvolvido em Haskell como trabalho acadêmico para a disciplina de Programação Funcional. Permite criar, modificar, organizar e consultar tarefas com categorias, prioridades e tags.

-> Funcionalidades
- ✅ Adicionar, remover e modificar tarefas
- ✅ Filtrar por status (Pendente/Concluída), prioridade e categoria
- ✅ Busca por palavras-chave e tags
- ✅ Gestão de prazos (dias restantes, tarefas atrasadas)
- ✅ Relatórios estatísticos
- ✅ Persistência em arquivo

-> Pré-requisitos
- [GHC](https://www.haskell.org/ghc/) (versão 8.10 ou superior)
- [Cabal](https://www.haskell.org/cabal/) (ou Stack)
- QuickCheck (versão 2.15.0.1)
- time (versão 1.14)
- directory (versão 1.3.9)
- Um arquivo nomeado como "arqteste.txt" (para as funções de teste de arquivo)
- Um arquivo .txt para as funções de persistência de dados

-> Clone o repositório
git clone https://github.com/seu-usuario/Trabalho1-Haskell.git && cd Trabalho1-Haskell

-> Instale a lib QuickCheck
cabal install --lib QuickCheck

-> Instale a lib time
cabal install --lib time

-> Instale a lib directory
cabal install --lib directory

-> Compilar o projeto
ghc --make Main.hs -o gerenciador-tarefas

-> Executar o programa
./gerenciador-tarefas

-> Executar testes com QuickCheck
Pode ser usado pela oção via menu no programa compilado. Código fonte esta no arquivo "ChecadaRapida.hs"
