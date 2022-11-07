{- |
Module      : Tarefa3_2021li1g061
Description : Representação textual do jogo
Copyright   : Luís Vítor Lima Barros <a100693@alunos.uminho.pt>;
            : Filipe Santos Gonçalves <a100696@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g061 where

import LI12122

instance Show Jogo where
  show = fun3 
 

{- | A função 'fun3' gera uma string a partir de um Jogo em que a cada "Peca" e "Jogador" corresponde um caracter específico

@
 Pontos a ter em atenção:
 
 *Visualmente cada sublista do mapa e jogador dados será separada pelo caraccter "\n" de modo a indicar que muda de linha  
 *Caso Jogador esteja a carregar uma caixa mas esteja na posiçao (x,0) a caixa não será inserida no jogo, apenas no jogador.
 *Nesta função, caso o Jogador carregue uma caixa, inserimos o Jogador no Mapa já em String como se não carregasse nenhuma caixa 
   usando a função 'insJogFalseMap' e depois inserimos a caixa no Mapa com o Jogador em string 
 *Caso o Jogador não carregue nenhuma caixa é só inseri-lo no Mapa já em String
 *Se o espaço onde se inserir o jogador não for vazio este será substituido pelo jogador
 *Estamos a assumir que o Mapa é valido e por isso as peças vêm de forma ordenada

@

==Exemplos de utilização:

>>>fun3 (Jogo [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
 ,[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
 (Jogador (5,2) Oeste True)) = 
 "      X\n     CX\nP   C<X\nXXXXXXX"

-}
fun3:: Jogo -> String
fun3 (Jogo [] _) = [] 
fun3 (Jogo m j)  = mudaLinhas (insFinal (representaMapa m) j )

{- | A função 'representaMapa' gera uma lista String com os caracteres apenas associados ao Mapa para isso usa a função 
 auxiliar 'representaMapa1' para cada sublista do Mapa 


@
 Condições necessárias: 
  
 *O Mapa tem de ser válido e vem de forma organizada 
    
@
-}
representaMapa:: Mapa -> [String]
representaMapa [] = [] 
representaMapa (h:t) = representaMapa1 h : representaMapa t 

{- | A função 'representaMapa1' gera uma string que associa um caracter específico a uma lista de peças 
@
 Caracteres associados a cada componente do tipo Peca
 
 *se a peça é "Vazio" o caracter associado é ' '
 *se a peça é "Caixa" o caracter associado é 'C'
 *se a peça é "Porta" o caracter associado é 'P'
 *se a peça é "Bloco" o caracter associado é 'X'
@

-}
representaMapa1:: [Peca] -> String
representaMapa1 [] = [] 
representaMapa1 (Vazio:t) = ' ' : representaMapa1 t
representaMapa1 (Caixa:t) = 'C' : representaMapa1 t
representaMapa1 (Porta:t) = 'P' : representaMapa1 t
representaMapa1 (Bloco:t) = 'X' : representaMapa1 t 

{- | A função 'insFinal' com uma lista String, irá ser usada a lista String associada ao Mapa, e um Jogador gera
lista uma String já com o caracter associado ao Jogador
-}
insFinal:: [String] -> Jogador -> [String]
insFinal [] _ = [] 
insFinal s (Jogador (x,y) d b) | b == False = insJogFalseMap s (Jogador (x,y) d b)
                               | b == True = insCaixaMap ( insJogFalseMap s (Jogador (x,y) d False) ) (Jogador (x,y) d b) 

{- | A função 'insJogFalseMap' dada uma lista String e um Jogador que com a componente Bool igual a False, ou seja que não carrega 
nenhuma Caixa, gera uma lista String com o caracter específico associado ao Jogador e para isso usa a função auxiliar 
'insJogFalseMap1' que a atua em cada sublista da lista String  
-}
insJogFalseMap:: [String] -> Jogador -> [String]
insJogFalseMap [] _ =  [] 
insJogFalseMap (h:t) (Jogador (x,y) d False) | y==0 = ((insJogFalseMap1 h (Jogador (x,y) d False)) : t )
                                             | otherwise = h : insJogFalseMap t (Jogador (x,y-1) d False)

{- | A função 'insJogFalseMap1' a partir de uma String e um Jogador com a componente Bool igual a False, que não carrega nenhuma 
Caixa gera uma String com o caracter associado ao Jogador

@
Caracteres associados ao Jogador:

 *se o Jogador tem a Direcao "Este" o caracter associado é '>'
 *se o Jogador tem a Direcao "Oeste" o caracter associado é '>'

@   

-}
insJogFalseMap1:: String -> Jogador -> String
insJogFalseMap1 [] _ = [] 
insJogFalseMap1 (h:t) (Jogador (x,y) d False) | x ==0 && d==Este = '>' : t 
                                              | x ==0 && d==Oeste = '<' : t
                                              | otherwise = h : insJogFalseMap1 t (Jogador (x-1,y) d False)

{- | A função 'insCaixaMap' a partir de uma lista String e um Jogador com a componente Bool igual a True gera uma lista String
com o caracter associado à Caixa que o Jogador carrega com o auxílio da função 'insCaixaMap1' que atua nas sublistas da lista 
String 
-}
insCaixaMap:: [String] -> Jogador -> [String]
insCaixaMap [] _ = []
insCaixaMap (h:t) (Jogador (x,y) d True) | y-1 == 0 = (insCaixaMap1 h (Jogador (x,y) d True) : t )
                                         | otherwise =  h : insCaixaMap t (Jogador (x,y-1) d True) 


{- | A função 'insCaixaMap1' a partir de uma String e um Jogador com compontente Bool igual a True, ou seja que carrega uma Caixa
gera uma String com o caracater associado à Caixa que o Jogador carrega

@
Caracteres associados à Caixa do Jogador:

 *se o Jogador tem o Bool "True" o caracter associado à Caixa é 'C'

@

-}
insCaixaMap1:: String -> Jogador -> String
insCaixaMap1 [] _ = [] 
insCaixaMap1 (h:t) (Jogador (x,y) d True) | x ==0 = 'C' : t 
                                          | otherwise = h : insCaixaMap1 t (Jogador (x-1,y) d True) 
                                          
{- | A função 'mudaLinhas' a partir de uma lista String gera uma String em que as sublistas da lista String serão separadas 
na String pelo caracter "\n" de forma a que no fim da lista este caracter não apareça
-}
mudaLinhas:: [String] -> String
mudaLinhas [] = [] 
mudaLinhas [h] = h 
mudaLinhas (h:t) = (h ++ "\n") ++ mudaLinhas t 