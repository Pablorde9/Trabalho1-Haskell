# Primeiro Trabalho
Gabriel Valentin, Pablo Rodrigues, Pedro de Colla, Ruan Pablo

## Roadmap 

- [X] Definicao dos Tipos Algebricos
- [X] funcoes basicas *
- [X] funcoes avancadas
- [X] gestao de prazos
- [X] sistema de tags
- [X] persistencia de dados
- [X] validacao
- [ ] interface simples via terminal
- [X] relatorios
- [X] testes
- [ ] quickcheck
- [X] modularizacao do codigo
- [X] readme organizado

*para as funcoes que podem retornar erro( como adcionar tarefas, vamo fazer uma funcao que compara se a entrada e igual o resultado e ela volta uma string para tela)

# Sistema de Gerenciamento de Tarefas em Haskell

Link para instalção: (https://www.haskell.org/img/haskell-logo.svg)

## Descrição
Sistema de gerenciamento de tarefas desenvolvido em Haskell como trabalho acadêmico para a disciplina de Programação Funcional. Permite criar, modificar, organizar e consultar tarefas com categorias, prioridades e tags.

## Funcionalidades
- ✅ Adicionar, remover e modificar tarefas
- ✅ Filtrar por status (Pendente/Concluída), prioridade e categoria
- ✅ Busca por palavras-chave e tags
- ✅ Gestão de prazos (dias restantes, tarefas atrasadas)
- ✅ Relatórios estatísticos
- ✅ Persistência em arquivo

## Pré-requisitos
- [GHC](https://www.haskell.org/ghc/) (versão 8.10 ou superior)
- [Cabal](https://www.haskell.org/cabal/) (ou Stack)

## Clone o repositório
git clone https://github.com/seu-usuario/Trabalho1-Haskell.git &&  
cd Trabalho1-Haskell.git

## Compilar o projeto
ghc --make Main.hs -o gerenciador-tarefas

## Executar o programa
./gerenciador-tarefas

## Executar testes com QuickCheck
nome arquivo do quick check
