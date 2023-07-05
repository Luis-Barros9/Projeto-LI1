{- |
Module      : Tarefa2_2021li1g061
Description : Construção/Desconstrução do mapa
Copyright   : Luís Vítor Lima Barros <a100693@alunos.uminho.pt>;
            : Filipe Santos Gonçalves <a100696@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g061 where

import LI12122
import Tarefa1_2021li1g061 (dimenX,dimenY,dimensoes)
{- | A função 'constroiMapa' realiza a construção de um mapa, assumindo que este é válido e que a partir de uma lista de pares em que a primeira 
componente é do tipo peça e a segunda as suas respetivas coordendas origina um mapa, isto é 
uma lista de listas de peças em que cada sublista está ordenada pela coordenada x e todas as peças 
de cada sublista têm a mesma coordenada y

==Exemplo de utilização: 

>>> constroiMapa  [ (Porta, (0, 3)),(Bloco, (0, 4)),(Bloco, (1, 4)),(Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4, 3)),(Bloco, (5, 4))
,(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))] = 
[[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
[Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
-}

constroiMapa :: [(Peca, Coordenadas)] -- ^ lista de pares (Peca,Coordenadas) inicial
        -> Mapa -- ^ mapa resultante 
constroiMapa [] = [] 
constroiMapa a = removeCord ( agrupaLinhas ( juntaPaC a t ) )
                    where t = mapa2 a 

{- | A função 'mapa2' cria uma lista, a partir da função auxiliar 'mapa1' de coordenadas por ordem crescente x e depois y com as dimensões 
do mapa que se pretende ,já que irá usar as funções auxiliares 'dimenX' e 'dimenY' para calcular as dimensões do
mapa pretendido

@
 Condições necessárias para se poder gera o mapa2:

 * A lista de (Peca, Coordenadas) não pode ser vazia, pois será impossível obter as dimensões de um mapa vazio
 * Ter pelo menos uma (Peca, Coordenadas) de modo a gerar um mapa só de coordenadas crescentes, a começar em (0,0) 
@


O objetivo desta função é futuramente comparar com a lista de (Peca,Coordenadas) para poder organizar a maneira como 
as peças serão introduzidas no mapa uma vez que a lista dada poderá não estar de forma crescente quando falando em coordenadas
-}
mapa2:: [(Peca,Coordenadas)] -> [Coordenadas] 
mapa2 a = mapa1 (dimenX a,dimenY a)

{- | A função 'mapa1' gera uma lista crescente por x e depois por y [(0,0),(1,0)..] com todas as coordenadas dadas as dimenções do mapa que se pretende construir
 -}
mapa1:: Coordenadas-> [Coordenadas]
mapa1 (ncolunas, nlinhas) = [(x,y) | y <- [0.. nlinhas] ,x <- [0..ncolunas ]]
                        
{- | A função 'juntaPaC' serve para organizar uma lista de (Peca,Coordenadas) por ordem que irá aparecer no mapa da maneira em
que primeiro organiza-se em ordem a "x" e depois em ordem a "y"

Caso duas peças diferentes tenham as mesmas coordenadas a que virá em último lugar substituíra a que 
apareceu previamente, caso se repitam peças iguais com as mesmas coordenadas não irá mudar nada, já que a função vai substituir
"a" por "a" ,ou seja terá o mesmo efeito
Também estamos a considerar que caso na lista de (Peca,Coordenadas) sejam omitidas as peças "Vazio" que se caso não sejam encontradas as coordenadas quando
comparando com a função "mapa2" origará uma peça "Vazio" com essas mesmas coordenadas

==Exemplos de utilização:

>>> juntaPaC [(Bloco,(0,1)),(Porta,(0,1)),(Caixa,(1,0))]
[(Vazio,(0,0)),(Caixa,(1,0),(Porta,(0,1)),(Vazio,(1,1))]
-}
juntaPaC:: [(Peca,Coordenadas)] -> [Coordenadas] -> [(Peca,Coordenadas)] --fazer uma lista do mapa com coordenadas crescentes (peca,(0,0))....
juntaPaC _ [] = []
juntaPaC ((peca,cord):t) (x:xs) = if cord == x then (peca,cord) : juntaPaC t xs 
                                  else  
                                        if elem x (removePecas t) then (tiraPeca t x) : juntaPaC ( deleteP (tiraPeca t x) ((peca,cord):t) ) xs
                                        else (Vazio,x) : juntaPaC ((peca,cord):t) xs 
juntaPaC [] (h:t) =  (Vazio,h) : juntaPaC [] t 

{- | A função 'agrupaLinhas' gera uma lista de listas em que cada sublista estará organizada em ordem a "x" de forma crescente e cada sublista estará também
organizada em relação a "y" de forma a que todas as sub listas possuam o "y" igual e a primeira sublista esteja caracterizada por "y" igual a 0, a segunda a 1 ...

Esta função será aplicada ao resultado da função anterior ('juntaPaC') 
-}                                
agrupaLinhas:: [(Peca,Coordenadas)] -> [[(Peca,Coordenadas)]] 
agrupaLinhas [] = []
agrupaLinhas [x] = [[x]]
agrupaLinhas ( (b,(x1,y1)):t ) | elem y1 (cordY (cabeca1 r) ) = ( (b,(x1,y1)):(cabeca1 r) ) : tail r 
                               | otherwise = [(b, (x1,y1))] : r
                                   where r = agrupaLinhas t 

{- | A função 'removeCord1' é bem simples, dada uma lista de (Peca,Coordenadas) origina uma lista de Peca pela mesma ordem da lista anterior, no fundo 
o nome da função refere o que ela faz, dada uma lista apenas remove as coordenadas
Esta função sera usada como auxiliar para quando pretendermos remover as coordenadas de uma lista de listas
-}
removeCord1:: [(Peca,Coordenadas)] -> [Peca]
removeCord1 [] = []
removeCord1 ((p,c):t) = p : removeCord1 t

{- | A função 'removeCord' é semelhante à anterior, no entanto em vez de atuar numa lista de (Peca,Coordenadas) atua numa lista de listas em que cada sublista 
é uma lista de (Peca,Coordenadas) e utiliza a função anterior para efetuar em cada sublista
-}
removeCord:: [[(Peca,Coordenadas)]] -> [[Peca]]
removeCord [] = [] 
removeCord (h:t) = removeCord1 h : removeCord t

{- | A função 'cordY' dada uma lista de (Peca,Coordenadas) apenas dá a coordenada de ordem Y do primeiro elemento da lista na forma de inteiro
-}
cordY:: [(Peca,Coordenadas)] -> [Int] 
cordY ((_,(x,y)):t) = [y]  

{- | A função 'cabeca1' dada uma lista de listas de (Peca,Coordenadas) dá a primeira sublista da lista de listas inicial
-}
cabeca1:: [[(Peca,Coordenadas)]] -> [(Peca,Coordenadas)]
cabeca1 (h:t) = h

{- | A função 'tiraPeca' dada uma lista de (Peca,Coordenadas) e umas coordenadas da um elemento da forma (Peca,Coordenadas) que é o primeiro elemento da lista
com coordenadas iguais às coordenadas dadas inicialmente
-}
tiraPeca:: [(Peca,Coordenadas)] -> Coordenadas -> (Peca,Coordenadas)
tiraPeca ((p,c):t) x | c == x = (p,c)
                     | otherwise = tiraPeca t x 

{- | A função 'removePecas' dada uma lista de (Peca,Coordenadas) forma uma lista apenas com as coordenadas que cada peça tinha, no fundo o nome da função resume
o que a mesma reproduz
-}
removePecas:: [(Peca,Coordenadas)] -> [Coordenadas] 
removePecas [] = []
removePecas ((_,x):t) = x : removePecas t 

{- | A função 'deleteP' dado um elemento da forma (Peca,Coordenadas) e uma lista de (Peca,Coordenadas) remove a primeira ocorrência desse elemento na lista
caso esse elemento não pertença à lista devolve a mesma lista
-}
deleteP::(Peca,Coordenadas)-> [(Peca,Coordenadas)]-> [(Peca,Coordenadas)] 
deleteP _ [] = []
deleteP x (h:t)
    | x == h = t
    | otherwise = h : deleteP x t
 

{- | A função 'desconstroiMapa' dado um Mapa devolve um lista da forma (Peca,Coordenadas), em que esta lista é composta pelos elementos do mapa e as suas
respetivas coordenadas, note-se que os elementos da forma "Vazio" são omitidos na lista criada, a função 'desconstroiMapa' é quase a função inversa da função 
'constroiMapa'

@Condições necessárias:

 *Não é preciso preocupar quanto à questão da organização do mapa já que este está organizado e cada sublista tem a mesma componente "y" e o "x" vem de forma 
 crescente começando pelo 0

 *É necessário inserir as coordenadas (0,0) na função desconstroiMapaAux1 para podermos percorrer todas as coordenadas e começando pela primeira
  visto que o mapa está de forma organizada

@

==Exemplo de utilização: 

>>> desconstroiMapa [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
[Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]] = 
    [ (Porta, (0, 3)),(Bloco, (0, 4)),(Bloco, (1, 4)),(Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4, 3)),(Bloco, (5, 4))
    ,(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))] 
-}
desconstroiMapa :: Mapa -- ^ mapa resultante da função 'constroiMapa' 
                -> [(Peca, Coordenadas)] -- ^ lista com os elementos introduzidos na função 'constroiMapa' com exceção dos elementos "Vazio" caso tenham sido introduzidos
desconstroiMapa a = desconstroiMapaAux1 a (0,0) 
     
{- | A função 'desconstroiMapaAux1' associa um mapa às suas respetivas coordenadas omitindo as peças da forma "Vazio" criando uma lista 
do tipo (Peca,Coordenadas) sabendo que o mapa vem organizado e por isso para cada sublista de peça usa a função auxiliar 'desconstroiMapaAux' para cada sublista
e para o resto das sublistas chama a recursividade e adciona 1 à componente "y" já que a próxima sublista irá ter as componentes iguais "y" somando uma unidade
-}
desconstroiMapaAux1:: Mapa -> Coordenadas -> [(Peca, Coordenadas)] 
desconstroiMapaAux1 [] _ = []
desconstroiMapaAux1 (h:t) (x,y) = desconstroiMapaAux h (x,y) ++ desconstroiMapaAux1 t (x,y+1)

{- | A função 'desconstroiMapaAux' associa uma lista de peças às suas respetivas coordenadas omitindo as peças da forma "Vazio" criando uma lista 
do tipo (Peca,Coordenadas) sabendo que o "y" dentro dessa lista é igual e o "x" começa pelo 0 e vai adicionando sempre 1 já que a próxima peça irá
estar na posição a seguir
Quando a lista de peças é vazia retorna uma lista vazia (caso de paragem)

-}
desconstroiMapaAux:: [Peca] -> Coordenadas -> [(Peca, Coordenadas)]
desconstroiMapaAux [] _ = [] 
desconstroiMapaAux (h:t) (x,y) | h == Vazio = desconstroiMapaAux t (x+1,y)
                               | otherwise = [(h,(x,y))] ++ desconstroiMapaAux t (x+1,y)


