module Tarefa4_2021li1g061_Spec where

import Test.HUnit
import LI12122
import Tarefa3_2021li1g061
import Tarefa4_2021li1g061
import Fixtures

testsT4 =
  test
    [ "Tarefa 4 - Teste Move m1e1 Oeste" ~: Jogo m1r (Jogador (5, 3) Oeste False) ~=?  moveJogador m1e1 AndarEsquerda
    , "Tarefa 4 - Teste Move m1e1 Este" ~: Jogo m1r (Jogador (6, 0) Este False) ~=?  moveJogador m1e1 AndarDireita
    , "Tarefa 4 - Teste Move m1e1 Trepar" ~: m1e1 ~=? moveJogador m1e1 Trepar
    , "Tarefa 4 - Teste Move m1e1 InterageCaixa" ~: m1e1 ~=?  moveJogador m1e1 InterageCaixa
    , "Tarefa 3 - Teste Move m1e3 InterageCaixa" ~: m1e4Inter ~=? moveJogador m1e4 InterageCaixa
    , "Tarefa 4 - Teste Move m1r InterageCaixa v2" ~: Jogo 
                                                      [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
                                                        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
                                                        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
                                                        [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
                                                        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
                                                      (Jogador (5,3) Oeste True) ~=? moveJogador (Jogo m1r (Jogador (5,3) Oeste False)) InterageCaixa

    , "Tarefa 4 - Teste movimentos m1e1" ~: m1e2 ~=?  correrMovimentos m1e1 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]
    , "Tarefa 4 - Teste movimentos m1e2 Caixa1" ~: Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (3, 3) Este True) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa]
    , "Tarefa 4 - Teste movimentos m1e2 Caixa2" ~:
      Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (2, 3) Oeste False) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa, AndarEsquerda, InterageCaixa]
    , "Tarefa 4, Teste movimentos m1e1 para m2e1"  ~: m2e1 ~=? 
               correrMovimentos m1e1 [AndarEsquerda,AndarDireita,InterageCaixa,AndarEsquerda,InterageCaixa,AndarEsquerda,AndarEsquerda,InterageCaixa,Trepar,AndarEsquerda,AndarDireita]
    ]