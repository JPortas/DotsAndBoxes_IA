# Dots And Boxes 1ª Fase
Este projeto passa pela implementação de algoritmos de pesquisa para a procurar de uma determinada solução para um problema neste jogo. Foi considerado que o jogo seria jogado apenas por um único jogador com o intuito de encontrar o melhor caminho para fechar X caixas dado um problema inicial e uma quantidade de caixas fechadas finais.
Para isto foram implementados os algoritmo BFS, DFS e A*.

## Executar

Para o desnvolvimento foi utilizado o SBCL com a consola do VSCode pelo no caso de estar instalado o SBCL é possível correr o projeto com:

```console
sbcl --load .\main.lisp --eval '(in-package #:main)' --eval '(main)'
```
para executar a função que lança a UI ou,
```console
sbcl --load .\main.lisp --eval '(in-package #:main)'
```
para executar o programa no geral e poder utilizar funções na consola.

## Ficheiros e Packages

Para estrutura e organização do trabalhou recorreu-se ao uso de packages do LISP e ficheiros que são referenciados na tabela abaixo. Em cada ficheiro colocou-se um package e deu se use dos necessários para ter acesso às funções necessárias para o desenvolvimento.

| Ficheiro | Package | Use |
| --- | :-----------: | :------: |
| **main.lisp** | `main` |  `board` `node` `not-informed-search` <br/> `informed-search` `example-values` <br/> `unit-tests` |
| **board.lisp** | `board` | N/a |
| **node.lisp** | `node` |  `board` |
| **not-informed-search.lisp** | `not-informed-search` | `node` |
| **informed-search.lisp** | `informed-search` | `node` |
| **informed-search.lisp** | `informed-search` | `node` |
| **example-values.lisp** | `example-values` | `node` |
| **unit-tests.lisp** | `unit-tests` | `board` `example-values` |

## Ficheiros de Dados

problemas.dat

##### main.lisp

É onde inicia o sistema e tem as funções que fazem ligação ao projeto todo. Contém a lógica para gerar a UI e guardar ficheiros.

##### board.lisp

Contem a lógica da manipulação dos estados relativamente ao tabuleiro de jogo, como ligar dois pontos, ver quantas caixas estão fechadas entre outras.

##### node.lisp

Contém a lógica para para a manipulação da informação de cada nó, como obter heurísticas, criar um nó, obter o estado que seria o tabuleiro...

##### not-informed-search.lisp

Implementa os algoritmos BFS e DFS e funções de suporte aos mesmos para a execução destes dois dado os parametros recebidos.

##### informed-search.lisp

Implemente o algoritmo A* que recebe uma heurística presente nos nodes.lisp para a execução do algoritmo.

##### example-values.lisp

Possuí funções que retornam os dados estruturados para utilização nos testes na consola para não haver a necessidade de escrever o código diretamente na consola para cada parametro da função a testar-se.

##### unit-tests.lisp

Seria a implementação de código para executar as funções e comparar o output esperado que poderia ser usado individualente para verificar se as funções estão a funcionar corretamente.
O código para estes testes não foi todo implementado pelo que está incompleto.

## Implementação dos Algoritmos

### BFS

O algoritmo BFS é implementado no ficheiro `not-informed-search.lisp` em é chamado através da função:
```lisp
(defun (bfs-init start-node closed-boxes-objective)
    ...
)
```
Esta função recebe como parametros o nó inicial que seria já no formato de nó em si, ao seja a lista constituida pelo (board (depth) (parent)).

Depois esta função chama...

### DFS

...

### A*

...

## Limitações

...

## Requesitos Não Implementados

* Algoritmo SMA*
* Algoritmo IDA*
* Algoritmo RBFS