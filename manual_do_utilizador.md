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

O ficheiro `problemas.dat` contém os problemas que o programa irá utilizar durante a sua execução.

Para adicionar novos problemas deve-se inserir uma lista com o tabuleiro e o número desejado de caixas fechadas e seguir a estruturação do ficheiro, ou seja, inserir esse problema na mesma linha que os outros problemas. Cada problema deve estar separado com / entre si.

**Exemplo:**
Ficheiro inicial:

```txt
((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 3 ) / ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 4)
```

Pretende-se introduzir o seguinte tabuleiro:

```txt
(
 ((0 0 0) (0 0 1) (0 1 1) (0 0 1)) 
 ((0 0 0) (1 1 1) (1 0 1) (0 1 1)) 
)
```

Com o objetivo de **5 caixas fechadas**.

Então devemos colocar isso dentro de uma lista da seguinte forma:

```txt
(
    (
        ((0 0 0) (0 0 1) (0 1 1) (0 0 1)) 
        ((0 0 0) (1 1 1) (1 0 1) (0 1 1)) 
    )
    5
)
```

E de seguida colocar no ficheiro, resultando no seguinte ficheiro:

```txt
((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 3 ) / ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 4) / ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (1 1 1) (1 0 1) (0 1 1))) 5)
```

Após a execução do programa irão ser necessários alguns dados do utilizador de acordo com o pedido. Esses dados devem ser números.

## Exemplo de aplicação

Após executar `(main "problemas.dat")` irá aparecer o seguinte ecrã:

```console
Welcome to IA 0.1 :)
Qual a posição do problema?
```

O utilizador deve introduzir a posição do problema (número).

Caso não exista nenhum problema nessa posição é novamente pedido ao utilizador a posição.

```console
Essa posição não existe
Qual a posição do problema?
```

O mesmo acontece caso o utilizador use letras em vez de números.

De seguida é pedido o algoritmo de procura:

```console
Que algoritmo quer usar para procurar? 
1- Procura na largura
2- Procura na profundidade
3- A*
```

*Nota* caso o valor seja maior que o permitido é assumido a última opção. Caso não seja um número é pedido outra vez uma opção.

Depois de escolhido o algoritmo o programa pede a profundidade caso seja **Procura em profundidade** e caso seja o **A Estrela**

```console
Qual a profundidade limite?
```

Por fim é mostrado o resultado e estatísticas de desempenho da busca com o algoritmo escolhido e essa busca é gravada no ficheiro `desempenho.dat`

```console
Board: (((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1)))
Objetivo:4
Algoritmo: DFS
Profundidade: 5
Resultado da procura: ((((1 0 0) (0 1 1) (0 1 1) (0 1 1))
                        ((0 0 0) (0 1 1) (0 1 1) (0 1 1)))
                       5
                       ((((1 0 0) (0 1 1) (0 1 1) (0 1 1))
                         ((0 0 0) (0 1 1) (0 0 1) (0 1 1)))
                        4
                        ((((1 0 0) (0 1 1) (0 1 1) (0 1 1))
                          ((0 0 0) (0 1 0) (0 0 1) (0 1 1)))
                         3
                         ((((1 0 0) (0 1 1) (0 1 1) (0 0 1))
                           ((0 0 0) (0 1 0) (0 0 1) (0 1 1)))
                          2
                          ((((1 0 0) (0 0 1) (0 1 1) (0 0 1))
                            ((0 0 0) (0 1 0) (0 0 1) (0 1 1)))
                           1
                           ((((0 0 0) (0 0 1) (0 1 1) (0 0 1))
                             ((0 0 0) (0 1 0) (0 0 1) (0 1 1)))
                            0 NIL))))))
Total de nós gerados: 3741
Total de nós expandidos: 307
Ramificação: 3729/307
Tabuleiro solução: (((1 0 0) (0 1 1) (0 1 1) (0 1 1))
                    ((0 0 0) (0 1 1) (0 1 1) (0 1 1)))
Evaluation took:
  0.065 seconds of real time
  0.062500 seconds of total run time (0.062500 user, 0.000000 system)
  95.38% CPU
  145,411,677 processor cycles
  4,128,608 bytes consed

NIL
```
