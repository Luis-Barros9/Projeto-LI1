module Tarefa6_2021li1g061_Spec where

import Test.HUnit
import LI12122
import Tarefa6_2021li1g061
import Tarefa4_2021li1g061
import Maps
import Fixtures


{- a função 'ResolveCheck', recebe uma lista de possiveis movimentos, e testa se estes resolvem um jogo, que é solucionável
dentro do limite de jogadas possíveis -}
resolveCheck :: Maybe [Movimento] -> Jogo -> Bool
resolveCheck Nothing (Jogo m (Jogador (x,y) _ _)) = False
resolveCheck (Just lmoves) j = let (Jogo mapa (Jogador (x,y) _ _)) = correrMovimentos j lmoves
                               in (mapa !! y) !! x == Porta

testsT6 = 
    test
       [ "Tarefa 6 - Testar movimenots negativos:  " ~: Nothing ~=?  resolveJogo (-1) m1e1
        ,"Tarefa 6 - Testar 0 movimentos desda Porta: " ~: Just [] ~=? resolveJogo 0 (Jogo m1r (Jogador (0,3) Oeste False))
        ,"Tarefa 6 - Testar 0 movimentos desda Porta: " ~: Nothing ~=? resolveJogo 0 m1e1
        ,"Tarefa 6 - Testar resolver jogo m1e1:" ~: Just [AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=? resolveJogo 6 m1e1
        ,"Tarefa 6 - Testar movimentos insufecients:" ~: Nothing ~=? resolveJogo 5 m1e1
        ,"Tarefa 6 - Testar resolver impossível:" ~: Nothing ~=? resolveJogo 10 m3Imp
        ,"Tarefa 6 - Testar resolver impossível com muitas jogadas:" ~: Nothing ~=? resolveJogo 100 m3Imp
        ,"Tarefa 6 - Testar resolver jogo impossível" ~:  True ~=? resolveCheck (resolveJogo 20 m3Poss) m3Poss
        ,"Tarefa 6 - Testar resolver nivel 3 movimentos insufecients" ~: Nothing ~=? resolveJogo 10 n3
        ,"Tarefa 6 - Testar resolver nivel 3 movimentos sufecients" ~: True ~=? resolveCheck (resolveJogo 21 n3) n3
       ]
