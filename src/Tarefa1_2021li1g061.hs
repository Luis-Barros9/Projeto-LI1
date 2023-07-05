{- |
Module      : Tarefa1_2021li1g061
Description : Validação de um potencial mapa
Copyright   : Luís Vítor Lima Barros <a100693@alunos.uminho.pt>;
            : Filipe Santos Gonçalves <a100696@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g061 where

import LI12122

{- | A função 'validaPotencialMapa'  recebe uma lista de peças e as suas coordenadas
e testa se estas correspondem a um mapa válido

@
 Condições necessárias para um mapa ser válido:

 * Não possuirmos duas peças com as mesmas coordenadas;
 * Possuir exatamente uma porta;
 * Uma caixa não pode estar a flutuar (têm de estar em cima de um bloco ou de outra caixa);
 * Têm de existir pelo menos um espaço vazio, declarao ou por omissão
 * Possuir um chão válido.

@

Para o mapa ser valído recorremos ao uso de funções auxiliares que cada uma verifica estas condições.
Assim  a função retorna True se todas estas funções auxiliares retornarem também True. Caso contrário devolve False
-}
validaPotencialMapa :: [(Peca, Coordenadas)] -- ^potencial mapa 
                       -> Bool -- ^validade do mapa
validaPotencialMapa potMap = 
    (posDiff potMap) && (unicaPorta potMap) && (flutuar potMap) && (espacosVazios potMap) && (baseCheck potMap)
-- verificar as 5 condições, têm  de ser todas verdadeiras

{- | A função 'posDiff' verifica, se todos  não existem peças com a mesma coordenada

== Exemplos de utilização:

>>> posDiff [(Bloco,(3,3)),(Porta,(4,4)),(Vazio,(3,3))]
False

>>> posDiff [(Porta,(3,3)),(Caixa,(2,2)),(Bloco,(2,1))]
-}


posDiff :: [(Peca, Coordenadas)] -> Bool -- verificar se há posições repetidas  *perguntar se aparecer duas vezes a mesma peça no mesmo sítio se devolve True ou False
posDiff potMap = posDiffAux potMap []  -- utilizar uma função auxiliar na qual vamos guardando no acumulador as coordenas anteriores
        where posDiffAux [] _ = True    -- se não houver mais nenhuma peça para tentar introduzir, e sinal que nao houve repetição, e assim retornar true
              posDiffAux ((_, coordenada):t) l | elem coordenada l = False 
                                               | otherwise = posDiffAux t (coordenada:l)

{- | A função 'unicaPorta' recebe o possível mapas e verifica se tem exatamente uma porta

== Exemplos de utilização:

>>> unicaPorta [(Bloco,(1,2)),(Caixa,(4,4))]
False

>>> unicaPorta [(Bloco,(1,2)),(Caixa,(4,4)),(Porta,(3,4))]
True

>>> unicaPorta [(Porta,(0,0)),(Bloco,(1,2)),(Caixa,(4,4)),(Porta,(3,4))]
False

-}
unicaPorta :: [(Peca, Coordenadas)] -> Bool
unicaPorta potMap = (length listaPortas) == 1 
             where  listaPortas = filter (\(peca,_)-> peca== Porta) potMap
                    -- funcao que conta o número de portas do potencial mapa


{- | A função 'flutuar' verifica se não existem Caixas a flutuar, ou seja, existindo uma Caixa,
 a peça que ocupa a posição imediatamente abaixo desta tem de ser ocupada por um Bloco ou outra Caixa

== Exemplo de utilização:

>>> flutuar [(Caixa,(0,3)),(Caixa, (0,4))]
False

>>> flutuar [(Caixa,(0,3)),(Bloco, (0,5))]
False

>>> flutuar [(Caixa,(0,3)),(Caixa,(0,4)),(Bloco, (0,5))]
True
-}
flutuar :: [(Peca, Coordenadas)] -> Bool --verifica se não há Caixas a flutuar (não tem um bloco por baixo) 
flutuar potMap = flutuarAux potMap []
        where -- função auxiliar que compara se existe uma caixa antes ou dps da lista de peças
              flutuarAux [] _ = True
              flutuarAux (a@(Caixa, (x,y)):t) listaPecas =   (any (\ x -> elem x t || elem x listaPecas) [(Bloco, (x,y+1)),(Caixa, (x,y+1))])  && flutuarAux t (a:listaPecas)
              flutuarAux (pecaNCaixa:t) listaPecas = flutuarAux t (pecaNCaixa:listaPecas) 

{- | A função 'espacosVazios' verifica se existe, pelo menos um espaço vazio no mapa dado.
 Este espaço vazio pode ser declarado de duas formas:
  
  * Por omissão, o utilizador introduz menos peças do que as que o mapa realmente tem
  * Declaradas diretamente pelo utilizador numa determinada posição

== Exemplo de utilização

>>> espacosVazios [(Porta,(0,0)),(Bloco,(0,1)),(Bloco,(1,1))]
True

>>> espacosVazios [(Porta,(0,0)),(Bloco,(0,1)),(Bloco,(1,1)),(Vazio,(1,0))]
True

>>> espacosVazios [(Porta,(0,0)),(Bloco,(0,1)),(Bloco,(1,1)),(Bloco,(1,0))]
False

-}
espacosVazios:: [(Peca, Coordenadas)] -> Bool 
espacosVazios [] = False  
espacosVazios potMap | length potMap < (x * y) = True 
                     | otherwise = any (\ (peca,_) -> peca == Vazio) potMap 
                        where (x,y) = dimensoes potMap                                   



{- | A função 'baseCheck' verifica se o mapa têm  um chão válido, ou seja se os últimos blocos de cada coluna 
possuem ligação entre eles

== Exemplos de utilização:

>>> baseCheck [(Bloco,(0,3)),Bloco,(1,3),(Bloco,(2,2)),(Bloco,(3,4)),(Bloco,(3,3))]
-}

{- | A função 'baseCheck' verifica se o mapa possui uma base de blocos válida. Recorrendo a funções auxiliares 
decompondo assim o problema em partes.

== Exemplos de utilização:

>>> baseCheck [(Bloco,(0,3)),(Bloco,(2,2)),(Bloco,(3,4)),(Bloco,(3,3))]
False

>>> baseCheck [(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,2)),(Bloco,(3,4))]
False

>>> baseCheck [(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,2)),(Bloco,(3,4)),(Bloco,(3,3)),(Bloco,(4,2))]
True

-}
baseCheck :: [(Peca, Coordenadas)] -> Bool -- funçao que verifica se a base do mapa é composta por blocos
baseCheck [] = False
baseCheck m = colsLigadas cols
    where cols = agrupaBlocosEmcols m
    
{- | A função 'dimensoes' recebe uma lista de peças e apartir dai calcula as dimensões,
ou seja, o número de colunas pelo número de linhas.

 __ Nota: o número de linhas e colunas correspondem a uma unidade acima da maior ordenada ou abcissa,respetivamente__

== Exemplo de utilização:

>>> dimensoes  [(Porta,(1,4)),(Bloco,(1,5)),(Porta,(2,0))]
(3,6)

>>> dimensoes [(Porta, (0, 3)),(Bloco, (1, 4)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))]
(7,5)

-}
dimensoes:: [(Peca, Coordenadas)] -> Coordenadas    
{-função que dá um par da dimensão x e dimensão y-}  
dimensoes a = ((dimenX a) +1,(dimenY a)+1)

{- | a função auxiliar 'dimenX' vai sendo utilizada ao longo das funções declaradas.
Esta determina a maior abcissa das peças dadas

== Exemplo de utilização:
>>> dimenX [(Porta,(1,4)),(Bloco,(1,5)),(Porta,(2,0))]
2

>>> dimenX   [(Porta, (0, 3)),(Bloco, (1, 4)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))]
6

== Propriedades:
prop>dimenX [(peca, (x,y))] = x
-}
dimenX:: [(Peca, Coordenadas)] -> Int 
dimenX [(_, (x1,y1))] = x1
dimenX ( (_, (x1,y1)) :t) = max x1 (dimenX t)


{- | a função auxiliar 'dimenY' vai ser utilizada como auxiliar nas restantes funções declaradas.
Esta determina a ordenada maxima das peças dadas

== Exemplo de utilização:
>>> dimenyY [(Porta,(1,4)),(Bloco,(1,5)),(Porta,(2,0))]
5

>>> dimenY   [ (Porta, (0, 3)),(Bloco, (1, 4)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))]
4

== Propriedades:
prop>dimenY [(peca, (x,y))] = y
-}
dimenY:: [(Peca, Coordenadas)] -> Int 
dimenY [(_, (x1,y1))] = y1
dimenY ( (_, (x1,y1)) :t) = max y1 (dimenY t) 
{- | Dada uma coluna de blocos, a função 'ultBlocoDaCol' retorna o bloco na posição mais abaixo.
Nota: esta função só é utilizada em colnas com pelo menos um bloco.
-}

ultBlocoDaCol :: [(Peca,Coordenadas)] -- ^coluna de blocos
                 -> (Peca,Coordenadas) -- ^último bloco da coluna
ultBlocoDaCol pecas@((_,(c,_)):t) = (Bloco, (c,linha))
        where linha = maximum (map (\ (_,(_,y)) -> y) pecas) -- linha máxima

{- | A função 'agrupaBlocosEmcols' recebe um mapa e apartir deste cria listas de blocos de cada coluna, sendo que
se uma coluna não tiver blocos, esta vai devolver uma lista vazia.

== Exemplos de utilização:

>>> agrupaBlocosEmcols [(Bloco,(0,3)),(Bloco,(0,2)),(Porta,(1,0)),(Bloco,(2,3))]
[[(Bloco,(0,3)),(Bloco,(0,2))],[],[(Bloco,(2,3))]]

>>> agrupaBlocosEmcols [(Bloco,(0,3)),(Bloco,(2,3)),(Bloco,(1,0)),(Bloco,(0,2))]
[[(Bloco,(0,3)),(Bloco,(0,2))],[(Bloco,(1,0))],[(Bloco,(2,3))]]
-}
agrupaBlocosEmcols :: [(Peca,Coordenadas)] -- ^potencial mapa
                      -> [[(Peca,Coordenadas)]] -- ^lista com apenas os blocos do mapa em cada coluna
agrupaBlocosEmcols m = agrupaAux 0 soBlocos
               where colmax = dimenX m
                     soBlocos = filter (\ (p,_) -> p == Bloco) m 
                     agrupaAux x blocos | x == colmax = [blocos]
                                        | otherwise = soColx : agrupaAux (x+1) semColx
                                    where soColx = filter (\ (p,(c,l)) ->  c == x) blocos
                                          semColx = filter (\ (p,(c,l)) ->  c /= x) blocos

{- | A função 'colsLigadas', verifica se todas as colunas do mapa têm pelo menos um bloco e
se tiverem, verifica se estas colunas estão todas ligadas, utilizando assim a função 'pecasLigadas' com colunas consecutivas como argumento
Nota: quando so temos uma coluna  não vazia então verifica-se a condição como válida
-}
colsLigadas :: [[(Peca,Coordenadas)]] -- ^lista das colunas do mapa
               -> Bool
colsLigadas [] = False
colsLigadas ([]:t) = False
colsLigadas (_:[]:t) = False
-- se uma coluna não tiver blocos, retornar false
colsLigadas [blocos] = True
colsLigadas (c1:c2:t) | pecasLigadas c1 c2 = colsLigadas (c2:t)
                      | otherwise = False

{- | A função 'pecasLigadas' verifica se os últimos blocos das duas colunas seguidas estão ligados.

 A ligação dos blocos é establecida de 3 formas:

 * os 2 blocos estão lado a lado, ou na diagonal;
 * se o último bloco da primeira coluna estiver mais abaixo do que o último da próxima, na primeira coluna têm de existir
 blocos em todas as linhas entre o último desta e o último bloco da próxima coluna;
 * se o último bloco da segunda coluna estiver mais abaixo do que o último da anterior, na segunda coluna têm de existir
 blocos em todas as linhas entre o último desta e o último bloco da primeira coluna;
-}
pecasLigadas :: [(Peca,Coordenadas)] -- ^coluna de blocos
                -> [(Peca,Coordenadas)] -- ^coluna de blocos seguinte
                -> Bool
pecasLigadas c1 c2 
          | elem y [b-1..b-1] = True
          | y > b = let linhasEntre = [b+1..y-1]
                 in all (\n -> elem (Bloco,(x,n)) c1) linhasEntre
          | otherwise = let linhasEntre = [y+1..b-1]
                 in all (\n -> elem (Bloco,(a,n)) c2) linhasEntre
    where (_,(x, y)) = ultBlocoDaCol c1
          (_,(a,b)) = ultBlocoDaCol c2

{-
pecasLigadas :: [(Peca,Coordenadas)] -- ^lista com todas as peças
                -> (Peca ,Coordenadas) -- ^ultimo bloco de uma coluna
                -> (Peca ,Coordenadas) -- ^ultimo bloco da coluna seguinte
                -> Bool -- ^validade
pecasLigadas mapa (_, (c,l)) (_, (x,y)) | elem l [y-1..y+1] = True -- se o segundo bloco esta na mesma linha do que o primeiro, ou na de baixo ou de cima
                                        | l > y = let linhasEntre = [y+1..l-1] -- como o bloco da coluna seguinte está mais acima, verificar se ha ligação pela coluna anterior
                                               in all (\ n -> elem (Bloco, (c,n)) mapa) linhasEntre
                                        | otherwise = let linhasEntre = [l+1..y-1] -- como o bloco da coluna seguinte está mais abaixo, verificar se ha ligação através da próxima coluna
                                               in all (\ n -> elem (Bloco, (x,n)) mapa) linhasEntre 
-}
