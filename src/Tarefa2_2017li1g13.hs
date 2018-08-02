{-|
Module      : Tarefa2_2017li1g13
Description : Tarefa 2 do projeto a desenvolver no âmbito da disciplina de LI1.
Copyright   : João Sampaio <a85318@uminho.pt>; José Ferreira <a83683@uminho.pt>

Tarefa 2 do projeto a desenvolver no âmbito da disciplina de LI1.
Contém a função valida que rebendo um Mapa verifica se este é válido ou não
-}
module Tarefa2_2017li1g13 where

import LI11718


-- | Conjunto de testes para a Tarefa 2 
testesT2 :: [Tabuleiro]
testesT2 = [
            [[]],
            [],
            [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],[Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
            [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0, Peca (Curva Este) 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 0, Peca (Curva Sul) 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
            [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],[Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],[Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
            [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 2, Peca Recta 2, Peca (Curva Este) 2, Peca Lava 0],[Peca Lava 0, Peca (Rampa Norte) 1, Peca Lava 0, Peca (Rampa Norte) 1, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
            [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0,Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 0, Peca (Curva Sul) 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
            [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 2, Peca Recta 2, Peca (Curva Este) 2, Peca Lava 0],[Peca Lava 0, Peca (Rampa Norte) 1, Peca Lava 0, Peca (Rampa Norte) 1, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Recta 1, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
            [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 1, Peca (Rampa Oeste) 0, Peca (Curva Este) 0, Peca Lava 0],[Peca Lava 0, Peca (Rampa Sul) 1, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 2, Peca (Rampa Oeste) 1, Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
            [[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
            [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca (Curva Norte) 1, Peca (Curva Este) 1,  Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 1, Peca (Rampa Norte) 0, Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca (Rampa Norte) 0, Peca Recta 0, Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 1, Peca Recta 1, Peca (Rampa Oeste) 0, Peca (Curva Sul) 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca (Rampa Este) 0, Peca (Curva Este) 1, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 1, Peca (Rampa Oeste) 0, Peca Recta 0, Peca (Curva Este) 0, Peca (Curva Norte) 0, Peca (Rampa Este) 0, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca (Rampa Sul) 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Recta 1, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca (Curva Oeste) 1, Peca (Curva Sul) 1,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
            [[Peca Lava 0, Peca Lava 0 , Peca Lava 0], [Peca Lava 0, Peca Recta 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0]],
            [[Peca Lava 0, Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0, Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0, Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0, Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0, Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
            [[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0,Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0,Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
            [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Este) 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
            [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
            [[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0]]
           ]

-- * Validar o mapa
-- | Função que recebe um Mapa e verifica se este respeita um conjunto de regras predefinidas
valida::Mapa -> Bool
valida m = isValidTabuleiro    m
        && isBorderLava        m
        && isValidPartidaPeca  m
        && isValidPercorreMapa m

-- ** Forma do Mapa
{- | Função que recebe um mapa e verifica se este é um Retângulo

   obs: Também verifica se o Mapa é um tabuleiro vazio

== Exemplo de utilização:
>>>isValidTabuleiro (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True

>>>isValidTabuleiro (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
False

>>>isValidTabuleiro (Mapa ((2,1),Este) [])
False

>>>isValidTabuleiro (Mapa ((2,1),Este) [[]])
False
-}
isValidTabuleiro:: Mapa -> Bool
isValidTabuleiro (Mapa ((x,y),o)  []    ) = False
isValidTabuleiro (Mapa ((x,y),o) [[]]   ) = False
isValidTabuleiro (Mapa ((x,y),o) [l]    ) = True
isValidTabuleiro (Mapa ((x,y),o) (h:m:t)) | length h == length m = isValidTabuleiro (Mapa ((x,y),o) (m:t))
                                          | otherwise            = False

-- ** Borda do Mapa
{- | Função que verifica se a borda de um Mapa é constituída apenas por (Peca Lava 0)

== Exemplo de utilização:
>>>isBorderLava (Mapa ((0,0),Sul) [[Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0]])
True

>>>isBorderLava (Mapa ((0,0),Sul) [[Peca Lava 0, Peca Recta 0],[Peca Lava 0, Peca Lava 0]])
False
-}
isBorderLava::Mapa -> Bool
isBorderLava (Mapa _ (h:t)) = all (== Peca Lava 0) h
                           && all (== Peca Lava 0) (last t)
                           && aux (init t) (length (init t))
                        where
                           aux:: [[Peca]] -> Int -> Bool
                           aux _ 0     = True
                           aux (h:t) x = head h == Peca Lava 0
                                      && last h == Peca Lava 0
                                      && aux t (x-1)

-- ** Partida do Mapa
-- | Função que recebe um Mapa e verifica se a Peca inicial e a imediatamnete antes são diferentes de Peca Lava 0
isValidPartidaPeca:: Mapa -> Bool
isValidPartidaPeca m@(Mapa (p,o) t) = pecaFinder m p                /= Peca Lava 0
                                   && pecaFinder m coorAntesPartida /= Peca Lava 0
                        where
                            coorAntesPartida = nudge p (orOposta o)

-- ** Percurso do Mapa
-- | Função que verifica se é possível percorrer um mapa
isValidPercorreMapa::Mapa -> Bool
isValidPercorreMapa m@(Mapa (p,o) t) | isValidNext m coorAntesPartida o = last listCoor == Just p && isAllLava (mapaDelete m (unique listCoor))
                                     | otherwise = False
                        where
                            nAvancos         = 2 * (nBlocosCaminho m + 1)
                            coorAntesPartida = nudge p (orOposta o)
                            listCoor         = percorreMapa m  (coorAntesPartida, o) nAvancos 1

{- | Função que recebe um Mapa , um par com uma Posicao e uma Orientacao e tenta, percorrendo esse mapa, chegar à partida 
     Se conseguir devolve uma lista de Posicao por onde passou
     Se não conseguir ao longo do processo avancar devolve na última posição da lista [Nothing]

     obs: Esta função tem certas particularidades

          *a Posicao que recebe são as coordenadas da Peca imediatamente antes da partida.

          *a Orientacao é a inicial

          *o primeiro Int é igual a 2 vezes o número de Blocos do Caminho mais um (a fim de a função não entrar num loop infinito)

          *o segundo Int é um acomulador que varia entre 0 e 1. Começa com o valor 1 e quando passa pela primeira vez na Partida passa para 0

== Exemplo de utilização:
>>>percorreMapa (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]) (1,1) Este 18 1
[Just (1,1),Just (2,1),Just (3,1),Just (3,2),Just (3,3),Just (2,3),Just (1,3),Just (1,2),Just (1,1),Just (2,1)]

>>>percorreMapa (Mapa ((1,1),Norte) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]) (1,2) Norte 10 1
[Just (1,2),Nothing]

-}
percorreMapa:: Mapa -> (Posicao, Orientacao) -> Int -> Int -> [Maybe Posicao]
percorreMapa m (p,o) 0 _ = [Nothing]
percorreMapa m@(Mapa (pi,oi) t) (p,o) v n | isValidNext m p o && coorNextPeca == pi && o == oi && n == 1 = Just p : percorreMapa m (coorNextPeca, dirChange m p o) (v - 1) (n - 1)
                                          | isValidNext m p o && coorNextPeca == pi && o == oi && n == 0 = [Just p, Just coorNextPeca]
                                          | isValidNext m p o                                            = Just p : percorreMapa m (coorNextPeca, dirChange m p o) (v - 1) n
                                          | otherwise                                                    = [Nothing]
            where
              coorNextPeca = nudge p o




-- *** Verificar a Próxima Peca

-- | Função que recebe um Mapa, a Posicao do carro nesse Mapa e a sua Orientacao e verifica se o carro se pode movimentar nessa Orientacao
isValidNext::Mapa -> Posicao -> Orientacao -> Bool
isValidNext m p o | tipoPeca == Recta       = isValidReta m p o
                  | tipoPeca == Curva Norte = isValidCurva m p o
                  | tipoPeca == Rampa Norte = isValidRampa m p o
                  | tipoPeca == Lava        = False
                    where
                      tipoPeca = tipoFinder (pecaFinder m (nudge p o))

-- **** Caso seja Reta
-- | Função que recebe um Mapa e uma Posicao e, assumindo que a próxima Peca é do tipo Recta, devolve o valor de verdade desse movimento
isValidReta:: Mapa -> Posicao -> Orientacao -> Bool
isValidReta m p o = finalAlturaFinder (pecaFinder m p) o == alturaFinder (pecaFinder m (nudge p o))

-- **** Caso seja Curva
-- | Função que recebe um Mapa, uma Posicao e uma Orientacao e, assumindo que a próxima Peca é do tipo Curva, devolve o valor de verdade desse movimento
isValidCurva:: Mapa -> Posicao -> Orientacao -> Bool
isValidCurva m p o = o == Norte && oCurva == Norte && aEntrada == aCurva
                  || o == Norte && oCurva == Este  && aEntrada == aCurva
                  || o == Sul   && oCurva == Sul   && aEntrada == aCurva
                  || o == Sul   && oCurva == Oeste && aEntrada == aCurva
                  || o == Este  && oCurva == Sul   && aEntrada == aCurva
                  || o == Este  && oCurva == Este  && aEntrada == aCurva
                  || o == Oeste && oCurva == Norte && aEntrada == aCurva
                  || o == Oeste && oCurva == Oeste && aEntrada == aCurva
                        where
                           oCurva    = orFinder (pecaFinder m (nudge p o)) o
                           aEntrada = finalAlturaFinder (pecaFinder m p) o
                           aCurva   = alturaFinder (pecaFinder m (nudge p o))

-- **** Caso seja Rampa
-- | Função que recebe um Mapa, uma Posicao e uma Orientacao e, assumindo que a próxima Peca é do tipo Rampa, devolve o valor de verdade desse movimento
isValidRampa:: Mapa -> Posicao -> Orientacao -> Bool
isValidRampa m p o =  orRampa == Norte && o == Norte && altEntrada == altRampa
                   || orRampa == Norte && o == Sul   && altEntrada == altRampa + 1
                   || orRampa == Sul   && o == Sul   && altEntrada == altRampa
                   || orRampa == Sul   && o == Norte && altEntrada == altRampa + 1
                   || orRampa == Este  && o == Este  && altEntrada == altRampa
                   || orRampa == Este  && o == Oeste && altEntrada == altRampa + 1
                   || orRampa == Oeste && o == Oeste && altEntrada == altRampa
                   || orRampa == Oeste && o == Este  && altEntrada == altRampa + 1
                        where
                            orRampa    = orFinder (pecaFinder m (nudge p o)) o
                            altEntrada = finalAlturaFinder (pecaFinder m p) o
                            altRampa   = alturaFinder (pecaFinder m (nudge p o))

-- *** Calculo da mudança de direção

-- | Função que recebe um Mapa, a Posicao do carro e a Orientacao em que o carro se desloca e devolve a Orientacao com que ele vai sair da próxima Peca
dirChange:: Mapa -> Posicao -> Orientacao -> Orientacao
dirChange m p o | tipoFinder (pecaFinder m (nudge p o)) == Curva Norte = dirChangeCurva m p o
                | otherwise                                            = o

-- **** Caso seja Curva
-- | Função que recebe um Mapa, a Posicao do carro e a Orientacao em que o carro se desloca e devolve a Orientacao com que ele vai sair da próxima Peca, assumindo que esta última é uma Curva
dirChangeCurva:: Mapa -> Posicao -> Orientacao -> Orientacao
dirChangeCurva m p o | o == Norte && orCurva == Norte && altEntrada == altCurva = Este
                     | o == Norte && orCurva == Este  && altEntrada == altCurva = Oeste
                     | o == Sul   && orCurva == Sul   && altEntrada == altCurva = Oeste
                     | o == Sul   && orCurva == Oeste && altEntrada == altCurva = Este
                     | o == Este  && orCurva == Sul   && altEntrada == altCurva = Norte
                     | o == Este  && orCurva == Este  && altEntrada == altCurva = Sul
                     | o == Oeste && orCurva == Norte && altEntrada == altCurva = Sul
                     | o == Oeste && orCurva == Oeste && altEntrada == altCurva = Norte
                                where
                                   orCurva    = orFinder (pecaFinder m (nudge p o)) o
                                   altEntrada = finalAlturaFinder (pecaFinder m p) o
                                   altCurva   = alturaFinder (pecaFinder m (nudge p o))

-- *** Número de Pecas não Peca Lava 0
{- | Função que devolve o número de Pecas que não são lava num Mapa
     obs: É calculando subtraindo ao número total de Pecas o número de (Peca Lava 0)

== Exemplo de Utilização:
>>>nBlocosCaminho (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]])
8
-}
nBlocosCaminho::Mapa -> Int
nBlocosCaminho x = areaTabuleiro x - nBlocosLava x

{- | Função que devolve o número de Pecas total num Mapa

== Exemplo de Utilização:
>>>areaTabuleiro (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]])
25
-}
areaTabuleiro::Mapa -> Int
areaTabuleiro (Mapa _ t) = length (head t) * length t

{- | Função que devolve o número de (Peca Lava 0) num Mapa

== Exemplo de Utilização:
>>>nBlocosLava (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]])
17
-}
nBlocosLava::Mapa -> Int
nBlocosLava (Mapa _ [])        = 0
nBlocosLava (Mapa (x,y) (h:t)) = listCount (Peca Lava 0) h + nBlocosLava (Mapa (x,y) t)

-- *** Testar função percorreMapa
{- | Função que vê se todo o Mapa é constituído por Peca Lava 0

== Exemplo de utilização:
>>>isAllLava (Mapa ((1,1),Este) [[Peca Lava 0, Peca Lava 0 ,Peca Lava 0],[Peca Lava 0, Peca Recta 0 ,Peca Lava 0],[Peca Lava 0, Peca Lava 0 ,Peca Lava 0]])
False

>>>isAllLava (Mapa ((1,1),Este) [[Peca Lava 0, Peca Lava 0 ,Peca Lava 0],[Peca Lava 0, Peca Lava 0 ,Peca Lava 0],[Peca Lava 0, Peca Lava 0 ,Peca Lava 0]])
True
-}
isAllLava:: Mapa -> Bool
isAllLava (Mapa (p,o) []) = True
isAllLava (Mapa (p,o) (x:xs)) = all (Peca Lava 0 ==) x && isAllLava (Mapa (p,o) xs)

{- | Função que coloca Pecas Lava 0 nas Posicoes de um Mapa que correspondem a uma lista de Posicao.
    Obs: Chama recursivamnete a função 'pecaDelete'

== Exemplo de utilização:
>>>mapaDelete (Mapa ((1,1),Este) [[Peca Lava 0, Peca Recta 0 ,Peca Lava 0],[Peca Lava 0, Peca Recta 0 ,Peca Lava 0],[Peca Lava 0, Peca Lava 0 ,Peca Lava 0]]) [Just (1,1),Just (1,0)]
(Mapa ((1,1),Este) [[Peca Lava 0, Peca Lava 0 ,Peca Lava 0],[Peca Lava 0, Peca Lava 0 ,Peca Lava 0],[Peca Lava 0, Peca Lava 0 ,Peca Lava 0]])
-}
mapaDelete:: Mapa -> [Maybe Posicao] -> Mapa
mapaDelete m [] = m
mapaDelete m [Just x]     = pecaDelete m x
mapaDelete m (Just x :xs) = mapaDelete (pecaDelete m x) xs

{- | Função que dada um Mapa e uma Posicao devolve um Mapa em que nessa Posicao a Peca foi substituida por uma Peca Lava 0

== Exemplo de utilização:
>>>pecaDelete (Mapa ((1,1),Este) [[Peca Lava 0, Peca Lava 0 ,Peca Lava 0],[Peca Lava 0, Peca Recta 0 ,Peca Lava 0],[Peca Lava 0, Peca Lava 0 ,Peca Lava 0]]) (1,1)
(Mapa ((1,1),Este) [[Peca Lava 0, Peca Lava 0 ,Peca Lava 0],[Peca Lava 0, Peca Lava 0 ,Peca Lava 0],[Peca Lava 0, Peca Lava 0 ,Peca Lava 0]])
-}
pecaDelete:: Mapa -> Posicao -> Mapa
pecaDelete (Mapa i t) (x,y) = Mapa i (take y t ++ [take x (t !! y) ++ [Peca Lava 0] ++ drop (x + 1) (t !! y)] ++ drop (y + 1) t)

-- ** Funções auxiliares

-- *** Finder

-- **** Peca
{- | Função que recebe uma Posicao e um Mapa e devolve a Peca que se encontra nessa Posicao

== Exemplo de utilização:
>>>pecaFinder (Mapa ((1,1),Este) [[Peca Lava 0, Peca Lava 0 ,Peca Lava 0],[Peca Lava 0, Peca Recta 0 ,Peca Lava 0],[Peca Lava 0, Peca Lava 0 ,Peca Lava 0]]) (1,1)
Peca Recta 0
-}                                        
pecaFinder::Mapa -> Posicao -> Peca
pecaFinder (Mapa _ t) (x,y) = (t !! y) !! x

{- | Função que recebe uma Peca e devolve o tipo tipado da Peca

== Exemplo de utilização:
>>>tipoFinder (Peca Recta 0)
Recta

>>>tipoFinder (Peca (Rampa Sul) 0)
(Rampa Norte) 
-}
tipoFinder:: Peca -> Tipo
tipoFinder (Peca Recta     _) = Recta
tipoFinder (Peca (Rampa _) _) = Rampa Norte
tipoFinder (Peca (Curva _) _) = Curva Norte
tipoFinder (Peca  Lava     0) = Lava

-- **** Altura
{- | Função que recebe uma Peca e devolve a sua Altura

== Exemplo de utilização:
>>>alturaFinder (Peca Recta 1)
1
-}
alturaFinder::Peca -> Altura
alturaFinder (Peca _ h) = h

{- | Função que recebe uma Peca e a Orientacao em que se viaja nessa Peca e devolve a Altura com que sai da Peca

'finalAlturaFinder' é diferente de 'alturaFinder'. A primeira calcula a altura com que o carro sai da Peca. A última só devolve a altura da Peca.

== Exemplo de utilização:
>>>finalAlturaFinder (Peca (Rampa Norte) 0) Norte
1

>>>finalAlturaFinder (Peca (Rampa Norte) 0) Sul
0
-}
finalAlturaFinder::Peca -> Orientacao-> Altura
finalAlturaFinder (Peca (Rampa o1) h) o2 | o1 == Norte && o2 == Norte = h + 1 
                                         | o1 == Norte && o2 == Sul   = h
                                         | o1 == Sul   && o2 == Sul   = h + 1
                                         | o1 == Sul   && o2 == Norte = h
                                         | o1 == Este  && o2 == Este  = h + 1
                                         | o1 == Este  && o2 == Oeste = h
                                         | o1 == Oeste && o2 == Oeste = h + 1
                                         | o1 == Oeste && o2 == Este  = h
finalAlturaFinder (Peca _ h) _ = h

-- **** Posicao
{- |Função que recebe uma Posição e uma Orientacao e devolve a Posicao que resulta em mover uma unidade na Orientacao dada

== Exemplo de utilização:
>>>nudge (1,1) Sul
(1,2)

>>>nudge (1,1) Este
(2,1)
-}
nudge:: Posicao -> Orientacao -> Posicao
nudge (x,y) d |d == Norte = aux (x  , y-1) 
              |d == Sul   = aux (x  , y+1)
              |d == Este  = aux (x+1, y  )
              |d == Oeste = aux (x-1, y  )
              where
                aux::(Int,Int) -> (Int,Int)
                aux (x,y) | x < 0 && y < 0 = (x+1 , y+1)
                          | x < 0          = (x+1 , y  )
                          | y < 0          = (x   , y+1)
                          |otherwise       = (x   , y  )

-- **** Orientacao
{- | Função que recebe uma Peca e caso esta seja do tipo Curva ou Rampa devolve a sua orientação.
     Caso contrário devolve devolve a Orientacao que lhe é dada

== Exemplo de utilização:
>>>orFinder (Peca (Curva Norte) 0) Norte
Norte

>>>orFinder (Peca Recta 0) Norte
Norte
-}
orFinder::Peca -> Orientacao -> Orientacao
orFinder (Peca (Curva o) _) _ = o
orFinder (Peca (Rampa o) _) _ = o
orFinder (Peca  Recta    _) o = o
orFinder (Peca  Lava     _) o = o

{- | Função que recebe uma orientação e devolve a Orientacao oposta

== Exemplo de utilização:
>>>orOposta Norte
Sul

>>>orOposta Este
Oeste
-}
orOposta::Orientacao -> Orientacao
orOposta o | o == Norte = Sul  
           | o == Sul   = Norte
           | o == Este  = Oeste
           | o == Oeste = Este

-- *** Listas genéricas

{- | Função que recebe uma lista e um elemento e devolve quantas vezes esse elemento ocorre na lista

== Exemplo de utilização:
>>>listCount 1 [1,2,3]
1

>>>listCount 1 [1,1,2,3]
2
-}
listCount:: Eq a => a -> [a] -> Int
listCount v l = length ( filter (v==) l)

{- | Função que recebe uma lista e devolve uma lista com os elementos repetidos eliminados

== Exemplo de utilização:
>>>unique [1,2,1,3,4]
[1,2,3,4] undefined 

>>>unique [1,2,3,1,2,3,4]
[1,2,3,4]
-}
unique:: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (deleter x xs)
            where
              deleter:: Eq a => a -> [a] -> [a]
              deleter _ [] = []
              deleter v (x:xs) | v == x    = deleter v xs
                               | otherwise = x : deleter v xs
