{- |
Module      : Tarefa6_2021li1g061
Description : Resolução de um puzzle
Copyright   : Luís Vítor Lima Barros <a100693@alunos.uminho.pt>;
            : Filipe Santos Gonçalves <a100696@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1g061 where

import LI12122
import Tarefa4_2021li1g061

{- | A funcção 'resolveJogo', recebe um número limite de jogadas e um jogo, e apartir daí tenta resolver o jogo
dentro do limite de jogadas, se for possível devolve a lista dos movimentos, se não for não devolve nada.

== Propriedades:
 Dado um número negativo de jogadas possíveis, qualquer que seja o jogo, esta não devolve uma lista de movimentos;

 Se tivermos um número de jogadas igual a 0, a função devolve uma lista de movimentos vazia se o jogador
já se encontrar na porta, caso contrário não devolve nada;

 Quando o número de jogadas máximo corresponde a um número positivo, esta função chama a função auxiliar 'tentarAux',
que determina a lista de soluções do jogo, se esta for vazia (length = 0) é sinal que o jogo não tem solução, caso contrário devolve a primeira solução
que a função auxiliar devolve.
-}
resolveJogo :: Int -- ^Número de movimentos maximo
               -> Jogo -- ^Estado inicial do jogo  
               -> Maybe [Movimento] -- ^Lista dos movimentos que resolvem o jogo ou nada
resolveJogo n _ | n < 0 = Nothing
resolveJogo n j
      | length lMoves == 0 = Nothing
      | otherwise = Just (head lMoves)
      where lMoves = tentarAux n [j] []

{- | A função 'tentarAux', recebe o número de movimentos disponíveis, o estado do jogo e uma lista de movimentos, 
 sendo que nesta vái se guardando os movimentos executados. Com isto, a função devolve uma lista com todas as listas de movimentos 
 que solucionam o jogo.

 == Propriedades:

 Quando o jogador já se encontra na porta, o jogo já está resolvido, devolvendo , assim, uma listaa lista de movimentos acumulada;
 
 Se o jogador não estiver na porta, e tiver 0 movimentos disponíveis, retorna uma lista Vazia, ou seja sem soluções para o jogo;
 
 Por outro lado, a função vai executar os 4 tipos de movimentos no jogo e adicioná los em último no acumulador, e diminuindo em 1 unidade os movimentos disponíveis.
 Desta forma, a função junta as lista de soluções apartir de cada movimento diferente, dos que chegam levam a um estado de jogo diferente
 de todos os estados até ao momento, de modo a evitar a executação movimentos
 que não chegam a fazer nada em concreto.
-}

tentarAux :: Int -- ^número de movimentos disponíveis
            -> [Jogo] -- ^estado do jogo até ao momento
            -> [Movimento] -- ^acumulador com a lista de movimentos 
            -> [[Movimento]] -- ^lista com soluções do mapa
tentarAux n jAnteriores@(j@(Jogo m (Jogador (x,y) _ _)):t) moves
          | posicao == Porta = [moves]
          | n == 0 = []
          | otherwise = tentaDir ++ tentaEsq ++ tentaInter ++ tentaTrep
      where andD = moveJogador j AndarDireita
            andE = moveJogador j AndarEsquerda
            inter = moveJogador j InterageCaixa
            trep = moveJogador j Trepar
            posicao = (m !! y) !! x
            tentaDir = if elem andD jAnteriores then [] else tentarAux (n-1) (andD:jAnteriores) (moves ++ [AndarDireita])
            tentaEsq = if elem andE jAnteriores then [] else tentarAux (n-1) (andE:jAnteriores) (moves ++ [AndarEsquerda])
            tentaInter = if elem inter jAnteriores then [] else tentarAux (n-1) (inter:jAnteriores) (moves ++ [InterageCaixa])
            tentaTrep = if elem trep jAnteriores then [] else tentarAux (n-1) (trep:jAnteriores) (moves ++ [Trepar])