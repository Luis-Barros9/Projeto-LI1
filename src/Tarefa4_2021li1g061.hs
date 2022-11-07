{- |
Module      : Tarefa4_2021li1g061
Description : Movimentação do personagem
Copyright   : Luís Vítor Lima Barros <a100693@alunos.uminho.pt>;
            : Filipe Santos Gonçalves <a100696@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g061 where

import LI12122
{- | A função 'moveJogador' realiza uma ação do jogador(andar; usar ou pegar numa caixa ; trepar algo),
  se for possível executar o movimento o jogo estado do jogo é alterado.

== Propriedades

prop>moveJogador jogo Trepar = trepar jogo
prop>moveJogador jogo InterageCaixa = interCaixa jogo
prop>moveJogador jogo AndarDireita = andarDir jogo
prop>moveJogador jogo AndarEsquerda = andarEsq jogo

-}  

moveJogador :: Jogo -- ^ jogo inicial
        -> Movimento -- ^ movimento a ser executado 
        -> Jogo -- ^ jogo resultante
moveJogador jogo move = 
    case move of Trepar -> trepar jogo
                 InterageCaixa -> interCaixa jogo
                 AndarDireita -> andarDir jogo
                 AndarEsquerda -> andarEsq jogo


{- | A função 'trepar'  verifica se o jogador está em condições de trepar, se estiver realiza o movimento,
se não estiver, retorna o jogo inicial.

@
 Condições necessárias para trepar:

 * Existir uma coluna, na direção do jogador;
 * Existir uma linha acima do jogador e ,caso este carregue uma caixa, existir uma linha 2 posições acima deste;
 * A peça ao lado, na direção do jogador, tem de ser trepável (caixa ou bloco) ,além disso em cima desta temos de
 ter em cima pelo menos 1 peça acessível (vazia ou porta) ,e 1 peça vazia acima da acessível se o jogador estiver carregar uma caixa;
 * A peça em cima do jogador for vazia, se este tiver caixa a peça 2 linhas acima também tém de ser vazia.
@

Caso pelo menos uma destas condições não se verificar, retornar o jogo inicial inalterado.
Por outro lado, se todas se verificarem, alterar a posição do jogador para uma linha acima e muda para a coluna imediatamente ao lado na sua direção.

-}
trepar :: Jogo -> Jogo
trepar j@(Jogo mapa (Jogador (x,y) direcao comCaixa))
    | comCaixa && direcao == Oeste = if (x == 0 || y <= 1) then j
        else if (elem pEsq trepavel) && (elem pEsqAcima acessivel) && all (\x -> x == Vazio) [pEsq2xAcima, pAcima, p2xAcima] 
             then Jogo mapa (Jogador (xAnt,yAcima1) direcao comCaixa)
             else j
    | comCaixa  = if (x == ultimaCol || y <= 1)  then j
        else if (elem pDir trepavel) && (elem pDirAcima acessivel) && all (\x -> x == Vazio) [pDir2xAcima, pAcima, p2xAcima]
                then Jogo mapa (Jogador (xSeg,yAcima1) direcao comCaixa)
             else j
    | direcao == Oeste = if (x == 0 || y == 0) then j
                        else if elem pEsq trepavel && (elem pEsqAcima acessivel) && pAcima == Vazio
                              then Jogo mapa (Jogador (xAnt,yAcima1) direcao comCaixa)
                             else j
    | otherwise = if (x == ultimaCol || y == 0) then  j
                  else if elem pDir trepavel && (elem pDirAcima acessivel) && pAcima == Vazio
                            then Jogo mapa (Jogador (xSeg,yAcima1) direcao comCaixa)
                       else j
    where trepavel = [Bloco, Caixa]
          acessivel = [Porta, Vazio]
          ultimaCol = (length (head mapa)) -1
          xSeg = x+1
          xAnt = x-1
          yAcima2 = y-2
          yAcima1 = y-1
          linha = mapa !! y
          linhaAcima1 = mapa !! yAcima1
          linhaAcima2 = mapa !! yAcima2
          pAcima = linhaAcima1 !! x
          p2xAcima = linhaAcima2 !! x
          pEsq = linha !! xAnt
          pEsqAcima = linhaAcima1 !! xAnt
          pEsq2xAcima = linhaAcima2 !! xAnt
          pDir = linha !! xSeg
          pDirAcima = linhaAcima1 !! xSeg
          pDir2xAcima = linhaAcima2 !! xSeg

{- | A função 'interCaixa', verifica se o jogador está em condiçoes de pegar ou pousar uma caixa, se estiver, 
realiza a ação desejada, se não estiver, retorna o jogo inicial.

@
 Condições necessárias para pousar a Caixa:

 * O jogador não se encontra na primeira linha do mapa;
 * Na direção do jogador tẽm de existir pelo menos uma coluna;
 * A peça  imediatamente ao lado ,na direção do jogador, e em cima desta são vazias;

 Condições necessároas para pegar na Caixa:

 * O jogador não se encontra na primeira linha do mapa;
 * Na direção do jogador têm de existir pelo menos uma coluna;
 * A peça imediatamente ao lado , na direção do jogador têm de ser uma caixa e em cima desta uma peça vazia;
 * Em cima do jogador temos de encontrar uma peça vazia.
@


Caso pelo menos uma destas condições não se verificar para a ação pretendida, retornar o jogo inicial inalterado.
Por outro lado, se o jogador conseguir pousar a caixa, largar esta na posição imediatamente ao lado, ou na peça Vazia mais abaixo desta
e alterar o estado deste para não portador de Caixa. Doutro modo, se o jogador reunir todas as condiçoés necessárias para pegar na caixa,
substituir a peça imediatamente ao lado , na direção do jogador,por uma Vazia e alterar o estado do jogador para portador de Caixa.
-}

interCaixa :: Jogo -> Jogo
interCaixa j@(Jogo mapa (Jogador (x,y) dir comCaixa))
    | comCaixa && dir == Oeste = if x == 0 || y == 0 then j
        else if pAcima == Vazio  -- verificar se é possível pousar a caixa
            then let mResultante = pousarCaixa (xAnt,yAcima1) mapa
                 in if mResultante == mapa then j
                    else Jogo mResultante (Jogador (x,y) dir False) -- jogador largou a caixa
        else j
    | comCaixa = if x == ultimaCol|| y == 0 then j
           else if pAcima == Vazio 
                then let mResultante = pousarCaixa (xSeg,yAcima1) mapa
                     in if mResultante == mapa then j 
                        else Jogo (pousarCaixa (xSeg,yAcima1) mapa) (Jogador (x,y) dir False)
                else j
    | dir == Oeste = if x == 0 || y == 0 then j
        else  if pEsq == Caixa && pEsqAcima == Vazio && pAcima == Vazio
                then Jogo (substituirEm (xAnt,y) Vazio mapa) (Jogador (x,y) dir True) -- o jogador está em condições de pegar na caixa à direita
                    else j

    | otherwise = if x == ultimaCol|| y == 0 then j
        else  if (pDir == Caixa && pDirAcima == Vazio && pAcima == Vazio)
              then Jogo (substituirEm (xSeg,y) Vazio mapa) (Jogador (x,y) dir True) -- o jogador está em condições de pegar na caixa
              else j
    where ultimaCol = length (head mapa) - 1
          xSeg = x+1
          xAnt = x-1
          yAcima1 = y-1
          linha = mapa !! y
          linhaAcima1 = mapa !! yAcima1
          pEsq = linha !! xAnt
          pEsqAcima = linhaAcima1 !! xAnt
          pDir = linha !! xSeg
          pDirAcima = linhaAcima1 !! xSeg
          pAcima =  linhaAcima1 !! x
{- | a função 'andarEsq' verifica a possibilidade de andar para a esquerda e se for possível,
altera a posição do jogador, para a posição determinada e altera a sua direção para Oeste.

@
 Condições necessárias para andar para a Esquerda:

 * o jogador não pode estar na primeira coluna, e além disso, se este for portador de caixa, não pode estar na primeira linha;
 * a peça à esquerda tem de ser Vazia ou a Porta, se o jogador carregar uma caixa, entao em cima desta peça temos de ter outra peça Vazia.
@

Se uma das condições não se verificar retornar o jogo inicial , com a direção do jogador para Oeste.
Por outro lado, direcionar o jogador para Oeste e mudar a sua posição para a posição adequada, de acordo com a função 'queda' utilizando como
argumentos a posição à esquerda deste e o mapa do jogo.
-}

andarEsq :: Jogo -- ^estado do jogo inical 
            -> Jogo -- ^estado do jogo após executar ou tentar o movimento para a esquerda
andarEsq j@(Jogo mapa (Jogador (x,y) dir comCaixa)) 
    | comCaixa = if x == 0 || y == 0 then casoErro
          else if elem pEsq acessivel && pEsqAcima == Vazio
                  then Jogo mapa (Jogador (queda (xAnt,y) mapa) Oeste comCaixa)
               else casoErro
    | otherwise= if x == 0 then casoErro
        else if elem pEsq acessivel
               then Jogo mapa (Jogador (queda (xAnt,y) mapa) Oeste comCaixa)
             else casoErro
    where acessivel= [Vazio, Porta]
          xAnt = x-1
          yAcima1 = y-1
          pEsq = (mapa !! y) !! xAnt
          pEsqAcima = (mapa !! yAcima1) !! xAnt
          casoErro = Jogo mapa (Jogador (x,y) Oeste comCaixa) 

{- | a função 'andarDir' verifica a possibilidade de andar para a direita e se for possível,
altera a posição do jogador, para a posição determinada alterando a sua direção para Este.

@
 Condições necessárias para andar para a Direita:

 * o jogador não pode estar na última coluna, e além disso, se este for portador de caixa, não pode estar na primeira linha;
 * a peça à direita tem de ser Vazia ou a Porta, se o jogador carregar uma caixa, entao em cima desta peça temos de ter outra peça Vazia.
@

Se uma das condições não se verificar retornar o jogo inicial.
Por outro lado, direcionar o jogador para Este e mudar a sua posição para a posição adequada, de acordo com a função 'queda' utilizando como
argumentos a posição à direita do jogador e o mapa do jogo.
-}

andarDir :: Jogo -- ^estado do jogo inicial
            -> Jogo -- ^estado do jogo após executar ou tentar o movimento para a direita
andarDir j@(Jogo mapa (Jogador (x,y) dir comCaixa)) 
    | comCaixa = if x == ultimaCol || y == 0 then casoErro
          else if elem pDir acessivel && pDirAcima == Vazio
                  then Jogo mapa (Jogador (queda (xSeg,y) mapa) Este comCaixa)
               else casoErro
    | otherwise= if x == ultimaCol then casoErro
        else if elem pDir acessivel
               then Jogo mapa (Jogador (queda (xSeg,y) mapa) Este comCaixa)
             else casoErro
    where acessivel= [Vazio, Porta]
          xSeg = x+1
          yAcima1 = y-1
          pDir = (mapa !! y) !! xSeg
          pDirAcima = (mapa !! yAcima1) !! xSeg
          ultimaCol = length (head mapa) -1
          casoErro = Jogo mapa ( Jogador (x,y) Este comCaixa)

{- | A função 'substituirEm' é uma função auxiliar da função 'interCaixa' , que altera o mapa do jogo,
 substituindo na coordenada do mapa pela posição desejada.

== Exemplos de utilização:

>>> substituirEm (1,1) Caixa [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Porta,Bloco,Bloco],[Bloco,Vazio,Vazio]]
[[Vazio,Vazio,Vazio],[Vazio,Caixa,Vazio],[Porta,Bloco,Bloco],[Bloco,Vazio,Vazio]]

>>> SubstituirEm (1,1) Vazio [[Vazio, Vazio, Vazio, Bloco],[Vazio,Caixa,Vazio,Porta],[Bloco,Bloco,Bloco,Bloco]]
[[Vazio, Vazio, Vazio, Bloco],[Vazio,Vazio,Vazio,Porta],[Bloco,Bloco,Bloco,Bloco]]

== Propriedades:

prop>substituirEm (c,l) peca [] = []
-}
substituirEm :: Coordenadas -- ^coordenadas do mapa onde vai ocorrer a substituição 
               -> Peca -- ^Peça a ser introduzida
               -> Mapa -- ^Mapa do jogo
               -> Mapa -- ^Mapa após a substituição
substituirEm _ _ [] = []
substituirEm (c,l) peca (h:t) | l == 0 = (substNaL c h peca) :t
                             | otherwise = h : substituirEm (c,l-1) peca t 
                where  substNaL n (h:t) peca | n == 0= peca : t
                                             | otherwise = h: substNaL (n-1) t peca

{- | A função 'pousarCaixa' é uma função auxiliar da função 'interCaixa', que determina a posição onde deve ser pousada a Caixa
 e , recorrendo à outra função auxiliar substituirEm', alterar a posição determinada por uma Caixa.

 Nota: Não podemos largar uma caixa em cima ou numa porta.

== Exemplos de utilização:

>>> pousarCaixa (1,0) [[Vazio, Vazio, Vazio, Bloco],[Vazio,Vazio,Vazio,Porta],[Vazio,Bloco,Bloco,Bloco],[Bloco,Bloco,Bloco,Bloco]]
[[Vazio,Vazio,Vazio,Bloco],[Vazio,Caixa,Vazio,Porta],[Vazio,Bloco,Bloco,Bloco],[Bloco,Bloco,Bloco,Bloco]]

>>> pousarCaixa (0,1) [[Vazio, Vazio, Vazio, Bloco],[Vazio,Vazio,Vazio,Porta],[Vazio,Bloco,Bloco,Bloco],[Bloco,Bloco,Bloco,Bloco]]
[[Vazio, Vazio, Vazio, Bloco],[Vazio,Vazio,Vazio,Porta],[Vazio,Bloco,Bloco,Bloco],[Bloco,Bloco,Bloco,Bloco]]
-}
--falta documentar exemplos
pousarCaixa :: Coordenadas -- ^posição provisória para a Caixa
               -> Mapa -- ^Mapa do jogo
               -> Mapa -- ^Mapa com a caixa já pousada na posição correta
pousarCaixa (x,y) mapa | (mapa !! y) !! x /= Vazio = mapa
                       | pecaAbaixo == Vazio = pousarCaixa (x,y+1) mapa
                       | pecaAbaixo == Porta = mapa
                       | otherwise = substituirEm (x,y) Caixa mapa
                       where pecaAbaixo = (mapa !! (y+1)) !! x


{- | A função 'queda' é uma função auxiliar das funções 'andarDir' e 'andarEsq'. Nesta,
dada a coordenada da posição imediatamente ao lado do jogador (direita ou esquerda respetivamente)
devolve a posição da peça Vazia mais abaixo possível desta. Determinando, assim, onde o jogador vai cair,
sendo esta utilizada para a nova posição do jogador no jogo.

== Exemplos de utilização

>>> queda (1,0) [[Bloco,Vazio,Vazio],[Bloco,Vazio,Bloco],[Porta,Vazio,Bloco],[Bloco,Bloco,Vazio]]
(1,2)

>>> queda (2,0) [[Bloco,Vazio,Vazio],[Bloco,Vazio,Bloco],[Porta,Vazio,Bloco],[Bloco,Bloco,Vazio]]
(2,0)

-}
queda :: Coordenadas -- ^posição provisória do jogador
         -> Mapa -- ^mapa do jogo
         -> Coordenadas -- ^posição resultante
queda (x,y) mapa | (mapa !! y) !! x == Porta = (x,y)
                 | elem ((mapa !! (y+1)) !! x) [Vazio,Porta] = queda (x,y+1) mapa
                 | otherwise = (x,y)

{- | A função 'correrMovimentos' executa uma lista de movimentos num jogo, para isto
utiliza a função 'moveJogador' para cada movimento na sua lista, indo assim atualizar o estado do jogo.

== Propriedades:

prop>correrMovimentos jogoi [] = jogoi
prop>correrMovimentos jogoi (h:t) = correrMovimentos (moveJogador jogoi h) t 
-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (h:t) =
    correrMovimentos (moveJogador jogo h) t
