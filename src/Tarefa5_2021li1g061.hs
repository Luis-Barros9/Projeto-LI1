{- |
Module      : Tarefa5_2021li1g061
Description : Aplicação Gráfica 
Copyright   : Luís Vítor Lima Barros <a100693@alunos.uminho.pt>;
            : Filipe Santos Gonçalves <a100696@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import Data.Char
import Tarefa4_2021li1g061
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import Graphics.Gloss.Interface.Pure.Game
import LI12122
import Maps
import Data.Maybe (fromJust)


-- | O jogo começa no menu Inicial na primeira opção
estadoJogoInicial :: EstadoJogo
estadoJogoInicial = Inicial Play


-- | O data type 'EstadoJogo' guarda que funcionalidade do jogo estamos
data EstadoJogo = Inicial MenuInicial  -- ^Menu Inicial
                | Segundo MenuPlay -- ^Menu De Jogo
                | MenuControlos -- ^Display dos controlos
                | Jogar Jogo Int -- ^Jogar Estado de Jogo e nível atual em índice
                | Venceu Int -- ^Jogador Venceu um nível
                | EscolherNivel Int -- ^MenuEscolherNivel

-- | O data type 'MenuPlay' guarda a opção do menu inicial iniciada
data MenuPlay = NewGame -- ^começar o jogo apartir do nível 1
              | LoadLevel -- ^começar o jogo apartir de um nível à escolha
              | Regressar -- ^Regressar ao menu Inicial

-- | O data type 'MenuInicial' guarda a opção do menu inicial selecionada
data MenuInicial = Play -- ^opção para ir para o menu de Jogo
                 | Controlos -- ^opção para ir para o menu dos controlos
                 | Exit -- ^fechar o jogo

-- | O tipo 'Textures' faz corresponder as peças com o seu bmp, exceto o vazio que não é necessário desenhar
type Textures = [(Peca , Picture)]



-- | O tipo 'VenceuJogo' guarda os bmps para os 2 tipos de 'VenceuJogo'
type VenceuJogo = (Picture,Picture)
-- | O tipo 'TexturaJogadores' faz corresponder cada Jogador com o seu bmp, de acordo com a sua direção e ter caixa ou não
type TexturaJogadores = [((Direcao, Bool), Picture)]



-- | O tipo 'EstadoGloss' guarda o estado atual e todos o bmps que vão ser utilizados 
type EstadoGloss = (EstadoJogo, -- ^EstadoAtual
                    Textures, -- ^lista que corresponde peças e bmps
                    Picture, -- ^bmp do background
                    TexturaJogadores, -- ^lisa que corresponde os tipos de jogadores e os bmps
                    Picture, -- ^bmp da seta
                    Picture, -- ^bmp do menu inicial
                    Picture, -- ^bmp do menu de jogo
                    Picture, -- ^bmp do menu dos controlos do jogo
                    Picture, -- ^bmp do menu de escolher nível
                    VenceuJogo -- ^bmps de vencer um jogo
                    )



-- | Display do jogo em tela cheia
dm::Display
dm = FullScreen


-- | Frame rate do jogo
fr::Int
fr = 50

-- | Medida do lado das peças e do jogador
l :: Float
l = 32

{- | A função 'fimDeJogo' recebe um jogo e verifica se o jogador venceu o jogo,
ou seja, se o jogador encontra-se na porta. 
-}
fimDeJogo :: Jogo -> Bool
fimDeJogo (Jogo mapa (Jogador (x,y) _ _)) = (mapa !! y) !! x == Porta


{- | A função 'evento' recebe um evento no teclado e aplica-o face ao estado do jogo, devolvendo assim o estado gloss
com o estado do jogo alterado ou não e as pictures intactas.
-}
evento :: Event -> EstadoGloss -> IO EstadoGloss
evento tecla (estJ,a,b,c,d,e,f,g,h,i) = do novoEstadoJogo <- event tecla estJ
                                           return (novoEstadoJogo,a,b,c,d,e,f,g,h,i)


{- | A função 'event' recebe um evento no teclado e aplica-o no estado de jogo, sendo que as teclas que não forem declaradas
não alteram o estado do jogo.
-}
event :: Event -> EstadoJogo -> IO EstadoJogo
--event (EventKey (SpecialKey KeyEnter) Down _ _) ()
event (EventKey (SpecialKey KeyUp) Down _ _) (Jogar jogo n) | fimDeJogo jogada = return (Venceu (n+1))
                                                            | otherwise = return $ Jogar jogada n
    where jogada = moveJogador jogo Trepar
event (EventKey (SpecialKey KeyDown) Down _ _) (Jogar jogo n) | fimDeJogo jogada = return (Venceu (n+1))
                                                            | otherwise = return (Jogar jogada n)
    where jogada = moveJogador jogo InterageCaixa
event (EventKey (SpecialKey KeyLeft) Down _ _) (Jogar jogo n) | fimDeJogo jogada = return (Venceu (n+1))
                                                            | otherwise = return (Jogar jogada n)
    where jogada = moveJogador jogo AndarEsquerda
event (EventKey (SpecialKey KeyRight) Down _ _) (Jogar jogo n) | fimDeJogo jogada = return (Venceu (n+1))
                                                            | otherwise = return (Jogar jogada n)
    where jogada = moveJogador jogo AndarDireita
event (EventKey (Char 'R') Down _ _) (Jogar _ n) = return (Jogar (niveis !! n) n)
event (EventKey (Char 'M') Down _ _) (Jogar _ n) = return $ Segundo NewGame
event (EventKey _ Down _ _) (Venceu n) | n == 10 = return estadoJogoInicial -- se vencer os níveis todos, voltar ao menu inicial
                   | otherwise = return (Jogar (niveis !! n) n)
event (EventKey (SpecialKey KeyUp) Down _ _) (Inicial Play) = return (Inicial Exit)
event (EventKey (SpecialKey KeyDown) Down _ _) (Inicial Play) = return (Inicial Controlos)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Inicial Play) = return (Segundo NewGame)
event (EventKey (SpecialKey KeyUp) Down _ _) (Inicial Controlos) = return (Inicial Play)
event (EventKey (SpecialKey KeyDown) Down _ _) (Inicial Controlos) = return (Inicial Exit)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Inicial Controlos) = return MenuControlos
event (EventKey (SpecialKey KeyUp) Down _ _) (Inicial Exit) = return (Inicial Controlos)
event (EventKey (SpecialKey KeyDown) Down _ _) (Inicial Exit) = return (Inicial Play)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Inicial Exit) = exitSuccess
event (EventKey _ Down _ _) (MenuControlos) = return (Inicial Play)
event (EventKey (SpecialKey KeyUp) Down _ _) (Segundo NewGame) = return (Segundo Regressar)
event (EventKey (SpecialKey KeyDown) Down _ _) (Segundo NewGame) = return (Segundo LoadLevel)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Segundo NewGame) = return (Jogar (niveis !! 0) 0)
event (EventKey (SpecialKey KeyUp) Down _ _) (Segundo LoadLevel) = return (Segundo NewGame)
event (EventKey (SpecialKey KeyDown) Down _ _) (Segundo LoadLevel) = return (Segundo Regressar)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Segundo LoadLevel) = return $ EscolherNivel 0
event (EventKey (SpecialKey KeyUp) Down _ _) (Segundo Regressar) = return (Segundo LoadLevel)
event (EventKey (SpecialKey KeyDown) Down _ _) (Segundo Regressar) = return $ Segundo NewGame
event (EventKey (SpecialKey KeyEnter) Down _ _) (Segundo Regressar) = return $ Inicial Play
event (EventKey (SpecialKey KeyUp) Down _ _) (EscolherNivel n) | n == 10 = return $ EscolherNivel 9
                                                                 | n < 5 = return $ EscolherNivel 10
                                                                 | otherwise= return $ EscolherNivel (n-5)
event (EventKey (SpecialKey KeyDown) Down _ _) (EscolherNivel n) | n == 10 = return $ EscolherNivel 0
                                                                 | n > 5 = return $ EscolherNivel 10
                                                                 | otherwise= return $ EscolherNivel (n+5)
event (EventKey (SpecialKey KeyRight) Down _ _) (EscolherNivel n) | n == 10 = return $ EscolherNivel 0
                                                                  | otherwise= return $ EscolherNivel (n+1)
event (EventKey (SpecialKey KeyLeft) Down _ _) (EscolherNivel n) | n == 0 = return $ EscolherNivel 10
                                                                 | otherwise= return $ EscolherNivel (n-1)
event (EventKey (SpecialKey KeyEnter) Down _ _) (EscolherNivel n) | n == 10 = return $ Segundo LoadLevel
                                                                  | otherwise = return $ Jogar (niveis !! n) n
event (EventKey (Char 'W') Down a b) s = event (EventKey (SpecialKey KeyUp) Down a b) s
event (EventKey (Char 'S') Down a b) s = event (EventKey (SpecialKey KeyDown) Down a b) s
event (EventKey (Char 'A') Down a b) s = event (EventKey (SpecialKey KeyLeft) Down a b) s
event (EventKey (Char 'D') Down a b) s = event (EventKey (SpecialKey KeyRight) Down a b) s
event (EventKey (Char ch) Down a b) s | isLower ch = event (EventKey (Char $ toUpper ch) Down a b) s
event _ estado = return estado



-- | A função 'getMapa' devolve o mapa de um jogo
getMapa::Jogo -> Mapa
getMapa (Jogo m _ ) = m

-- | A função 'getJogador' devolve o jogador de um jogo
getJogador::Jogo -> Jogador
getJogador (Jogo _ j) = j

-- apenas tou a testar algo
estadoGlossInicial :: Textures -- ^bmps das peças de jogo
                   -> Picture -- ^bmp do background
                   -> TexturaJogadores -- ^bmps dos tipos de jogadores
                   -> Picture -- ^bmp seta
                   -> Picture -- ^bmp menu inical
                   -> Picture -- ^bmp menu jogo
                   -> Picture -- ^bmp menu Nivel
                   -> Picture -- ^bmp menu Controlos
                   -> VenceuJogo -- ^bmps para o fim de um jogo
                   ->EstadoGloss -- ^estadoGloss inical
estadoGlossInicial textures b players s mI mJ mN mC v= (estadoJogoInicial, textures, b, players,s,mI,mJ,mN,mC,v)

{- | A função 'desenhaEstadoGloss' desenha cada um dos estados de jogo utilizando as pictures nessecárias
-}
desenhaEstadoGloss :: EstadoGloss -- ^estado gloss atual
                      -> IO Picture -- ^representação do estado gloss
desenhaEstadoGloss (EscolherNivel n, _, _, _, seta, _, _, mN, _, _) = return $ Pictures [mN,setadesenhada]
    where setadesenhada = setaMN n seta
desenhaEstadoGloss (MenuControlos, _, _, _, _, _, _, _, mC, _) = return mC
desenhaEstadoGloss (Segundo opcao, _, _, _, seta, _, mJ, _, _, _) = return $ Pictures [mJ,setadesenhada]
    where setadesenhada = setaMJ opcao seta
desenhaEstadoGloss (Inicial opcao, _, _, _, seta, mI, _, _, _, _) = return $ Pictures [mI,setadesenhada]
    where setadesenhada = setaMI opcao seta
desenhaEstadoGloss (Venceu n, _, _, _, _, _, _, _, _, (venceN,venceJ)) | n == 10 = return venceJ
                                                                       | otherwise = return venceN
desenhaEstadoGloss (Jogar jogo n, textures,background, player, _, _, _, _, _, _) = return $ Pictures [background, jogodesenhado]
    where desenhoMapa = desenhaMapa 0 0 mapa textures
          mapa        = getMapa jogo
          jogador = getJogador jogo
          caixa   = fromJust  (lookup Caixa textures )
          desenhoJogador = desenhaJogador jogador caixa player
          (c,h) = inicio mapa
          coef = scaleN !! n
          jogodesenhado = scale coef coef (Translate c h $ Pictures (desenhoMapa ++ desenhoJogador))


{- | A função 'desenhaJogador' transoforma o jogador em estado picture para ser utilizado ao desenhar o jogo 
-}
desenhaJogador :: Jogador -- ^Jogador do Jogo
                  -> Picture -- ^picture da caixa
                  -> TexturaJogadores -- ^lista que corresponde o tipo de jogador e a sua picture
                  -> [Picture] -- ^Lista de pictures do jogador, e caso carregue, a caixa em cima deste
desenhaJogador (Jogador (x,y) dir comCaixa) caixa textura | comCaixa = [picJogador, picCaixa]
                                                          | otherwise = [picJogador]
                                  where transacaoX = (fromIntegral x)*l
                                        transacaoY = -(fromIntegral y)*l 
                                        bmpJogador = fromJust (lookup (dir, comCaixa) textura)
                                        picJogador = Translate transacaoX transacaoY bmpJogador
                                        picCaixa = Translate transacaoX (transacaoY+l) caixa

{- | A função 'inicio' determina o as coordenadas necessárias para fazer a translação do jogo desenhado, de modo a
que este esteja corretamente centrado no mapa
  
-}
inicio :: Mapa -- ^mapa do jogo que vai ser corrido 
          -> (Float,Float) -- ^coordenadas iniciais para começar a desenhar o jogo
inicio mapa = (((-0.5)* comp), ( alt * 0.5))
   where nlinhas = length mapa
         ncolunas = length (head mapa)
         comp = (fromIntegral ncolunas) * l
         alt = (fromIntegral nlinhas) * l

{- | A função 'desenhaMap' devolve a lista das pictures das peças do mapa não Vazias, 
desnhando cada linha de uma vez e juntando-as, desenhando a próxima linha imediatamente abaixo -}
desenhaMapa :: Float -- ^transação X a ser executada
               -> Float -- ^transação Y a ser executada
               -> Mapa -- ^mapa do Jogo
               -> Textures -- ^lista que corresponde as peças, com as suas pictures
               -> [Picture] -- ^lista das pictures de todas as peças não vazias do mapa
desenhaMapa x y (h:t) textures = desenhaLinha x y h textures ++ desenhaMapa x (y-l) t textures
desenhaMapa x y _ _ = [] 


{- | A função 'desenhaLinha' desenha as peças não Vazia da linha do mapa, juntando-as numa lista, sendo que a medida que percorre esta,
aumenta a transação x para desenhar o resto da lista imediatamente à esquerda -}
desenhaLinha:: Float -- ^transação X a ser executada
               -> Float -- ^transação Y a ser executada 
               -> [Peca] -- ^linha do mapa
               -> Textures -- ^lista que corresponde as peças, com as suas pictures
               -> [Picture] -- ^lista das pictures de todas peças  não nulas da linha
desenhaLinha x y (h:t) textures | h == Vazio = restolinha
                                | otherwise = peca1 : restolinha
                                where peca1 = desenhaPeca x y h textures 
                                      restolinha = desenhaLinha (x+l) y t textures
desenhaLinha _ _ _ _  = []


-- | A função 'desenhaPeca' faz corresponder a Peca com o seu bmp e desenha-o na posição necessária

desenhaPeca:: Float -> Float -> Peca -> Textures -> Picture
desenhaPeca x y peca textures = Translate x y texture
    where texture   = fromJust  (lookup peca textures )


-- | A função 'setaMI' dado um menu inicial, devolve a picture da seta a apontar para a opção a ser escolhida
setaMI :: MenuInicial -- ^opção do menu inical
          -> Picture -- ^picture da seta
          -> Picture -- ^seta a Apontar para a opção ecolhida
setaMI Play bmp = Translate (-300) (220) $ scale 2 2 bmp
setaMI Controlos bmp = Translate (-450) (20) $ scale 2 2 bmp
setaMI Exit bmp = Translate (-250) (-190) $ scale 2 2 bmp

-- | A função 'setaMJ' dado um menu de jogo, devolve a picture da seta a apontar para a opção a ser escolhida
setaMJ :: MenuPlay -- ^opção de do menu de jogo
          -> Picture -- ^picture da seta
          -> Picture -- ^seta a apontar para a opção ecolhida
setaMJ NewGame bmp = Translate (-450) (260) $ scale 2 2 bmp
setaMJ LoadLevel bmp = Translate (-600) (0) $ scale 2 2 bmp
setaMJ Regressar bmp = Translate (-450) (-260) $ scale 2 2 bmp

-- | A função 'setaMI' dado , devolve a picture da seta a apontar para a opção a ser escolhida
setaMN :: Int -- ^número da opção escolhida no menu de escolher nível 
          -> Picture -- ^picture da seta
          -> Picture -- ^seta a apontar para opção escolhida
setaMN 10 bmp = Translate (-450) (-400) seta
   where seta = scale 2 2 bmp
setaMN n bmp | n < 5 = translate (x + fromIntegral (n*aumento)) y seta
    where seta = scale 1 0.75 $ rotate 90 bmp
          x = -620
          y = 370
          aumento = 310
setaMN n bmp = translate (x + fromIntegral (nMod*aumento)) y seta
    where seta = scale 1 0.75 $ rotate 270 bmp
          x = -620
          y = -300
          aumento = 310
          nMod = n-5

-- | A função 'reageTempoGloss' mantém o estado gloss inalterável com o passar do tempo
reageTempoGloss :: Float -> EstadoGloss -> IO EstadoGloss 
reageTempoGloss _ s = return s 


{- Na função 'main' encontramos o que o programa vai executar logo quando iniciado, carrega as imagens, e introduz las no estado
gloss inicial para ser utilizado na função 'playIO' que faz correr o jogo.  
-}
main :: IO ()
main =  do
    bloco <- loadBMP "Bloco.bmp"
    caixa <- loadBMP "Caixa.bmp"
    porta <- loadBMP "Porta.bmp"
    background <- loadBMP "Background.bmp" 
    jogEsem <- loadBMP "BonecoE.bmp"
    jogDsem <- loadBMP "BonecoD.bmp"
    jogEcom <- loadBMP "Boneco_seguraE.bmp"
    jogDcom <- loadBMP "Boneco_seguraD.bmp"
    controlos <- loadBMP "MenuControlos.bmp"
    menuI <- loadBMP "MenuInicial.bmp"
    menuJ <-loadBMP "MenuPlay.bmp"
    menuN <- loadBMP "MenuNiveis.bmp"
    venceuN <- loadBMP "CompletouNivel.bmp"
    venceuJ <- loadBMP "VenceuJogo.bmp"
    seta <- loadBMP "seta.bmp"
    let estado =
          (estadoGlossInicial 
           [(Bloco, bloco),
           (Caixa, caixa),
           (Porta, porta)
           ] 
           background
           [((Oeste, True),jogEcom),
            ((Oeste, False), jogEsem),
            ((Este, True),jogDcom),
            ((Este, False),jogDsem)
           ]
           seta
           menuI
           menuJ
           menuN
           controlos
           (venceuN,venceuJ)
           )
    playIO dm
     white
     fr
     estado
     desenhaEstadoGloss
     evento
     reageTempoGloss

