# **Projeto Nº 2:** Época Normal

<hr/>

![capa](./pics/capa.png)

<hr/>

**Unidade Curricular: Inteligência Artificial**
* **Professor**:
    * Eng. Filipe Mariano

* **Alunos**:

  * João Portas - 202002475
  * Lucas Freixieiro - 202002193

<br>

# Introdução

Este programa foi elaborado para a unidade curricular de inteligência artificial e tem o propósito de colocar em pratica o aprendido durante as aulas.
Este programa tem como objetivo simular o jogo Dots and Boxes, havendo então 2 modos de jogo, CPU vs Humano e CPU vs CPU.
As jogadas do CPU são feitas com base no algoritmo alfabeta.
O algoritmo alfabeta vai permitir ao CPU avaliar as suas jogadas e tentar assim decidir a que vai ter melhor resultados.

<br>

# Instalação e utilização

Para o desenvolvimento foi utilizado o SBCL com a consola do VSCode.
Se tiver o SBCL instalado é possível correr o projeto com:

```console
sbcl --load .\jogo.lisp --eval '(in-package #:jogo)'
```

para executar o programa no geral e poder utilizar funções na consola.
De seguida basta escrever `(start)` na consola e temos o programa a correr.

<br>

# Input/Output

Com o programa a correr o utilizador vai ter de inserir na consola o modo de jogo (usando valores numéricos).

O ficheiro `log.dat` serve para monitorizar as jogadas e o estado do jogo.
Este ficheiro vai conter os seguintes atributos:
 - Jogada realizada (i.e. (3 1 arco-horizontal))
 - O novo estado
 - Número de nós analisados
 - Número de cortes efetuados (de cada tipo)
 - Tempo gasto

<br>

# Exemplo de aplicação

Após executar `(start)` irá aparecer o seguinte ecrã:

```console
1. Human vs AI
2. AI vs AI
3. Quit

Digit an options and press ENTER:
```

O utilizador deverá escolher o modo de jogo.

Caso coloque uma opção inválida irá aparecer: `Invalid entered option! Try again.` e o utilizador deverá escolher outra opção.

## Opção 1:

Este modo de jogo será entre a AI e o utilizador.
Começa por pedir ao utilizador para escolher quem quer que faça a primeira jogada:

```console
******************* -[ Human vs AI Mode ]- *******************
Wich one should start? Enter (1) for you and (2) for IA:
```

Depois de escolhido quem começa primeiro o jogo começa:
***Nota este exemplo é do jogador a começar 1º - O utilizador é identificado por P1***

```console
■   ■   ■   ■   ■   ■   ■

■   ■   ■   ■   ■   ■   ■

■   ■   ■   ■   ■   ■   ■

■   ■   ■   ■   ■   ■   ■

■   ■   ■   ■   ■   ■   ■

■   ■   ■   ■   ■   ■   ■
Score P1: 0
Score P2: 0
Your turn (i.e. h21 or v11):
```

Agora o jogador deverá introduzir a sua jogada no seguinte formato: `direcao linha coluna` (sem espaços), sendo direcao entre [h;v], linha entre [n;n+1] e coluna até m.
***Nota: as linhas e colunas variam de acordo com a direção - o exemplo dado é para horizontais. Para verticais as linhas seriam até n e as coluna entre [m;m+1]***
Ou seja, para se colocar uma linha horizontal na 1ª coluna na 2ª linha seria: `h21`, que resultava no seguinte tabuleiro:

```console
■   ■   ■   ■   ■   ■   ■

■───■   ■   ■   ■   ■   ■

■   ■   ■   ■   ■   ■   ■

■   ■   ■   ■   ■   ■   ■

■   ■   ■   ■   ■   ■   ■

■   ■   ■   ■   ■   ■   ■
Score P1: 0
Score P2: 0
```

## Opção 2:

É um modo de jogo automático onde 2 AI's (CPU's) competem entre si para ver qual consegue fechar o maior número de caixas.
Irá aparecer algo deste gênero:

```console
******************* -[ AI vs AI Mode ]- *******************

■   ■   ■   ■   ■   ■   ■
                             
■   ■   ■   ■   ■   ■   ■
                             
■   ■   ■   ■   ■   ■   ■
                             
■   ■   ■   ■   ■   ■   ■
                             
■   ■   ■   ■   ■   ■   ■
                             
■   ■   ■   ■   ■   ■   ■
Score AI1: 0
Score AI2: 0
```

No fim aparecerá o vencedor:
```console
■---■───■---■───■---■───■
│   ¦   ¦   ¦   ¦   ¦   ¦    
■---■───■---■───■---■───■
¦   │   │   │   │   │   │    
■---■───■---■───■---■───■
¦   ¦   ¦   │   ¦   ¦   ¦    
■---■───■---■───■---■───■
│   │   │   │   ¦   │   │    
■---■───■---■───■---■───■
│   │   ¦   │   │   │   │    
■---■───■---■───■---■───■
AI 1 closed a box! Wait for her to play again...
AI1 Wins!
Final Score AI1: 18
Final Score AI2: 12
```

***Nota: O que aparece na consola pode ser consultado no ficheiro `log.dat`***

## Opção 3:
Esta opção serve para abandonarmos o programa