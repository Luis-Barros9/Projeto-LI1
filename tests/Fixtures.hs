module Fixtures where

import LI12122

m1,m2,m4,mImp,mFaq :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),(Bloco, (6, 1))
  ]
m2 = 
  [ (Porta,(0,3)),(Bloco,(0,4)),(Bloco,(1,4)),
    (Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4)),
    (Bloco,(5,4)),(Bloco,(6,4)),(Bloco,(6,3)),
    (Bloco,(6,2)),(Bloco,(6,1)),(Caixa,(4,3))
  ]


m4 = 
  [ (Bloco, (1,0)), (Bloco, (2,0)), (Bloco, (3,0)),(Bloco, (4,0)),(Bloco, (5,0)),(Bloco, (6,0)),(Bloco, (7,0)),(Bloco, (8,0)),(Bloco,(9,0)),
    (Bloco, (0,1)), (Bloco, (10,1)), (Bloco, (0,2)),(Bloco, (10,2)),(Bloco, (0,3)),(Bloco, (10,3)),(Bloco, (0,4)),(Bloco,(4,4)),(Bloco,(6,4)),
    (Caixa, (7,4)),(Caixa, (8,4)), (Bloco, (10,4)),(Bloco, (0,5)),(Porta, (1,5)),(Bloco,(3,5)),(Bloco,(5,5)),(Bloco,(7,5)),(Caixa,(8,5)),(Bloco,(9,5)),
    (Bloco, (10,5)),(Bloco,(1,6)),(Bloco,(2,6)),(Bloco,(5,6)),(Bloco,(8,6)),(Bloco,(9,6))
  ]

mImp = 
  [ (Porta, (0,1)),
    (Bloco, (0,2)),
    (Bloco, (1,3)),
    (Bloco, (2,1)),
    (Bloco, (3,2)),
    (Caixa, (4,2)),
    (Bloco, (4,3)),
    (Bloco, (5,2))
  ]

mFaq = 
 [ (Bloco,(5,0)),(Bloco,(6,0)),(Bloco,(7,0)),(Bloco,(12,0)),(Bloco,(13,0)),
   (Bloco,(14,0)),(Bloco,(15,0)),(Bloco,(16,0)),(Bloco,(17,0)),(Bloco,(18,0)),(Bloco,(19,0)),
   (Bloco,(20,0)),(Bloco,(1,1)),(Bloco,(2,1)),(Bloco,(3,1)),(Bloco,(4,1)),(Bloco,(8,1)),(Bloco,(9,1)),
   (Bloco,(10,1)),(Bloco,(11,1)),(Bloco,(21,1)),(Bloco,(0,2)),(Bloco,(21,2)),(Bloco,(0,3)),(Bloco,(21,3)),
   (Bloco,(0,4)),(Bloco,(21,4)),(Bloco,(0,5)),(Bloco,(6,5)),(Bloco,(21,5)),(Bloco,(0,6)),(Bloco,(6,6)),
   (Bloco,(21,6)),(Bloco,(0,7)),(Bloco,(6,7)),(Caixa,(7,7)),(Caixa,(8,7)),(Caixa,(9,7)),(Caixa,(10,7)),
   (Bloco,(21,7)),(Bloco,(0,8)),(Porta,(1,8)),(Bloco,(5,8)),(Bloco,(6,8)),(Bloco,(7,8)),(Bloco,(8,8)),
   (Bloco,(9,8)),(Bloco,(10,8)),(Bloco,(11,8)),(Bloco,(21,8)),(Bloco,(0,9)),(Bloco,(1,9)),(Bloco,(3,9)),
   (Bloco,(4,9)),(Bloco,(5,9)),(Bloco,(11,9)),(Bloco,(12,9)),(Bloco,(14,9)),(Caixa,(20,9)),(Bloco,(21,9)),
   (Bloco,(1,10)),(Bloco,(3,10)),(Bloco,(12,10)),(Bloco,(14,10)),(Bloco,(15,10)),(Caixa,(19,10)),(Caixa,(20,10)),
   (Bloco,(21,10)),(Bloco,(1,11)),(Bloco,(3,11)),(Bloco,(12,11)),(Bloco,(14,11)),(Bloco,(15,11)),(Caixa,(18,11)),
   (Caixa,(19,11)),(Caixa,(20,11)),(Bloco,(21,11)),(Bloco,(1,12)),(Bloco,(2,12)),(Bloco,(3,12)),(Bloco,(12,12)),
   (Bloco,(14,12)),(Bloco,(15,12)),(Bloco,(16,12)),(Bloco,(17,12)),(Bloco,(18,12)),(Bloco,(19,12)),(Bloco,(20,12)),
   (Bloco,(21,12)),(Bloco,(12,13)),(Bloco,(13,13)),(Bloco,(14,13))
 ]



m1r,m1Inter,m2r,m3r,m4r,mFaqr :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m1Inter =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m2r =  
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Caixa, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

-- [(Bloco, (1,0)),(Bloco, (2,0)),(Bloco, (3,0)),(Bloco, (4,0)),(Bloco, (5,0)),(Bloco, (6,0)),(Bloco, (0,1)),(Bloco, (7,1)),(Porta,(0,2)),(Bloco,(0,3)),(Caixa,(7,3)),(Bloco,(0,4)),(Bloco, (4,4)),(Bloco, (6,4)),(Bloco, (7,4)),(Bloco, (0,5)),(Bloco, (1,5)),(Bloco, (2,5)),(Bloco, (3,5)),(Bloco, (4,5)),(Bloco, (6,5)),(Bloco, (2,6)),(Bloco, (3,6)),(Bloco, (5,6)),(Caixa,(3,4))]

m3 = 
  [ (Bloco, (1,0)),(Bloco, (2,0)),(Bloco, (3,0)),(Bloco, (4,0)),(Bloco, (5,0)),(Bloco, (6,0)),
    (Bloco, (0,1)),(Bloco, (7,1)),(Porta,(0,2)),(Bloco,(0,3)),(Caixa,(7,3)),(Bloco,(0,4)),(Bloco, (4,4)),
    (Bloco, (6,4)),(Bloco, (7,4)),(Bloco, (0,5)),(Bloco, (1,5)),(Bloco, (2,5)),(Bloco, (3,5)),
    (Bloco, (4,5)),(Bloco, (6,5)),(Bloco, (2,6)),(Bloco, (3,6)),(Bloco, (5,6)),(Caixa,(3,4))
  ]

-- [[Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio],[Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa],[Bloco, Vazio, Vazio, Caixa, Bloco, Vazio, Bloco, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Vazio],[Vazio, Vazio, Bloco, Bloco, Vazio, Bloco, Vazio, Vazio]]

m3r = 
  [   [Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio],
      [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
      [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa],
      [Bloco, Vazio, Vazio, Caixa, Bloco, Vazio, Bloco, Bloco],
      [Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Vazio],
      [Vazio, Vazio, Bloco, Bloco, Vazio, Bloco, Vazio, Vazio]
  ]

m4r =
  [ [Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco],
    [Bloco,Porta,Vazio,Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Bloco,Bloco],
    [Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Vazio]
  ]

mFaqr =
  [ [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
    [Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Porta,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
    [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
    [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],
    [Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
    [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ]

m1e1,m1e2,m1e3,m1e4,m2e1,m3Poss,m3Imp :: Jogo

m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

m1e3 = Jogo m1r (Jogador (2, 3) Oeste True)

m1e4 = Jogo m1r (Jogador (5,3) Oeste True)

m1e4Inter = Jogo m1Inter (Jogador (5,3) Oeste False)

m2e1 = Jogo m2r (Jogador (1,3) Este False)

m3Poss = Jogo m3r (Jogador (6, 3) Este False)

m3Imp = Jogo m3r (Jogador (5,5) Oeste False)

