module Tarefa1_2021li1g061_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g061
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa m4r" ~: validaPotencialMapa m4 ~=? True
    , "Tarefa 1 - Teste Valida Mapa Faq" ~: validaPotencialMapa mFaq ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa posições iguais" ~: validaPotencialMapa [(Porta,(1,0)),(Bloco,(1,0)),(Bloco,(1,1)),(Bloco,(1,0))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa sem Vazios" ~: validaPotencialMapa [(Bloco,(0,1)) ,(Bloco,(1,0)),(Porta,(0,0)),(Caixa,(1,0))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa sem chão" ~: validaPotencialMapa mImp ~=? False
    , "Tarefa 1 - Teste Valida Mapa com chão" ~: validaPotencialMapa ((Bloco,(1,2)):mImp) ~=? True
    ]
