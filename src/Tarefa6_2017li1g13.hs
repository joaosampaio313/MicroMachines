{-|
Module      : Tarefa6_2017li1g13
Description : Módulo da Tarefa 6 para LI1 17/18

Módulo para a realização da Tarefa 6 de LI1 em 2017/18.
-}
module Tarefa6_2017li1g13 where

import LI11718
import Data.Maybe
import Tarefa2_2017li1g13
import Tarefa3_2017li1g13
import Tarefa4_2017li1g13

{-|
Função usada para simular um /bot/ no jogo /Micro Machines/.
Em cada instante, dado o tempo decorrido, o estado do jogo
e o identificador do jogador, toma uma ação.
-}
bot :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado atual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao   -- ^ a decisão tomada pelo /bot/
bot tick e j = Acao { acelerar = acel, travar = trav, esquerda = esq, direita = dir, nitro = nitDecide e j}  
            where
                (acel, trav) = cruiseSpeed e j
                (esq , dir ) = cruiseAng   e j

-- * Acelerador/Travão
-- | Função que devolve se o carro deve acelerar ou travar com base num 'Jogo' e no identificador do 'Carro'
cruiseSpeed:: Jogo -> Int
                   -> (Bool,Bool) -- ^ o Primeiro corresponde a se o 'Carro' deve acelerar ou não, o segundo a se deve Travar ou não
cruiseSpeed j i | vel >= emergency = (False,True)
                | vel >= target    = (False,False)
                | vel <  target    = (True ,False)
            where
                (target,emergency) = speedCalc j i
                (a, vel)   = vetCoorToAng (velocidade (carros j !! i))

-- | Função que calcula qual é 
speedCalc:: Jogo -> Int -> (Double,Double)
speedCalc j i | isCurva currPeca = (1,2)
              | otherwise        = (2,3)
            where
                currPeca = pecaFinder (mapa j) (cx,cy)
                (cx,cy)  = head carHist
                carHist  = historico j !! i
-- * Nitro
-- | Função que decide se o carro aplica nitro sobre ele mesmo 
nitDecide:: Jogo -> Int -> Maybe Int
nitDecide j i | isCurva currPeca = Nothing
              | vel < 1.9          = Just i
              | otherwise        = Nothing
            where
                currPeca = pecaFinder (mapa j) (head carHist)
                carHist  = historico j !! i
                (a, vel)   = vetCoorToAng (velocidade (carros j !! i))

-- * Volante
-- | Função que decide se o 'Carro' curva à direita ou à esquerda com base no estado do 'Jogo'
cruiseAng:: Jogo -> Int
                 -> (Bool,Bool)-- ^ o Primeiro corresponde a se o 'Carro' deve virar à Esquerda ou não, o segundo a se deve virar à Direita ou não
cruiseAng j i | a > target + trim && a < target + 180 - trim = (False,True )
              | a < target - trim && a > target - 180 + trim = (True ,False)
              | otherwise                      = (False,False)
            where
                target   = angChange (angDes j i) currPeca
                trim     = 3
                carHist  = historico j !! i
                (cx,cy)  = head carHist
                currPeca = pecaFinder (mapa j) (cx,cy)
                (a, n)   = vetCoorToAng (velocidade (carros j !! i))

-- | Função que devolve com que 'Angulo' é que um 'Carro' devia sair de uma 'Peca' com base no angulo em que este entra nesta
angChange:: Angulo -> Peca -> Angulo
angChange a p | tipoFinder p == Curva Norte = angChangeCurva a p
              | otherwise                   = a

-- | Função que devolve com que 'Angulo' é que um 'Carro' devia sair de uma 'Peca', assumindo que esta é uma Curva, com base no angulo em que este entra nesta
angChangeCurva:: Angulo -> Peca -> Angulo
angChangeCurva a p | a == 90  && orCurva == Norte  = 0
                   | a == 90  && orCurva == Este   = 180
                   | a == 270 && orCurva == Sul    = 180
                   | a == 270 && orCurva == Oeste  = 360
                   | a == 0   && orCurva == Sul    = 90
                   | a == 0   && orCurva == Este   = 270
                   | a == 180 && orCurva == Norte  = 270
                   | a == 180 && orCurva == Oeste  = 90
                   | otherwise                     = 0
                                where
                                   Peca (Curva orCurva) h = p 

{- | Função que  calcula o 'Angulo' com que o 'Carro' está a viajar.
Note que o cálculo é feito com base no historico do 'Carro'.
-}
angDes:: Jogo -> Int -> Angulo
angDes j i | length carHist == 1 = angPartida
           | otherwise = aux j i
            where
                angPartida = orToAng (orPartida (mapa j))
                carHist = historico j !! i
                aux::Jogo -> Int -> Angulo
                aux j i = desToAng (cx - px, cy - py)
                              where
                                  angPartida = orToAng (orPartida (mapa j))
                                  carHist = historico j !! i
                                  (cx,cy) = head carHist
                                  (px,py) = carHist !! 1




-- |Função que converte o deslocamento de uma Peca para outra para a 'Orientacao' em que essa viagem é feita
desToAng:: (Int,Int) -> Angulo
desToAng (0 , y)  | y > 0 = 270
                  | y < 0 = 90
desToAng (x , 0)  | x > 0 = 0
                  | x < 0 = 180
desToAng (1 , y)  | y > 0 = 270
                  | y < 0 = 90
desToAng (x , 1)  | x > 0 = 0
                  | x < 0 = 180
desToAng (-1 , y) | y > 0 = 270
                  | y < 0 = 90
desToAng (x , -1) | x > 0 = 0
                  | x < 0 = 180


                
-- * Funções auxiliares
-- ** Sobre Mapas
{- | Função que devolve a 'Orientacao' inicial de um 'Mapa'
==Exemplo de Utilização:
>>> Mapa (Este,(0,0)) [[Peca Recta 0]]
Este
-}
orPartida:: Mapa -> Orientacao
orPartida (Mapa (_,o) _) = o

