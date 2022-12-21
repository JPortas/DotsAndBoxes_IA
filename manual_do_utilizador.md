# Capa

* Unidade Curricular: Inteligência Artificial
* Alunos:

  * João Portas - 202002475
  * Lucas Freixieiro - 202002193

## Acrónimos

1. BFS - Breadth first search é um algoritmo de procura em espaço de estados que procura um nó objetivo em largura primeiro.
2. DFS - Depth first search é um algoritmo de procura em espaço de estados que procura um nó objetivo em profundidade primeiro.
3. A* - A estrela é um algoritmo de procura em espaço de estados que usa heurísticas e é um algoritmo informado.

## Introdução

Este programa foi elaborado para a unidade curricular de inteligência artificial e tem o propósito de colocar em pratica o aprendido durante as aulas.
Este programa tem como objetivo descobrir uma solução para um tabuleiro de Dots and Boxes jogado por um só jogador, a solução é um número de caixas vazias.
Para a descoberta da solução vai-se utilizar algoritmos de procura em espaço de estados, como o BFS, o DFS e o A*.

## Instalação e utilização

Para o desnvolvimento foi utilizado o SBCL com a consola do VSCode pelo no caso de estar instalado o SBCL é possível correr o projeto com:

```console
sbcl --load .\main.lisp --eval '(in-package #:main)'
```

para executar o programa no geral e poder utilizar funções na consola.
De seguida basta escrever `(main "problemas.dat")` na consola e temos o programa a correr.

## Input/Output
