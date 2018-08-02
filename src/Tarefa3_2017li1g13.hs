{-|
Module      : Tarefa3_2017li1g13
Description : Tarefa 3 do projeto a desenvolver no âmbito da disciplina de LI1.
Copyright   : João Sampaio <a85318@uminho.pt>; José Ferreira <a83683@uminho.pt>

Tarefa 3 do projeto a desenvolver no âmbito da disciplina de LI1.
Contém a função movimenta que calcula o estado de um carro ao fim de x tempo num tabuleiro.
-}
module Tarefa3_2017li1g13 where

import LI11718
import Tarefa2_2017li1g13
import Data.List

-- | Reta definida Vetorialmente por um par contendo um ponto da reta e o seu vetor dirtor
type RetaV = (Ponto, Velocidade)
-- | Par contendo dois pontos
type ParPontos = (Ponto, Ponto)
-- | Par que define um vetor
type Vetor = (Double , Double)

-- | Responsável pelos Testes da Tarefa 3

testesT3 :: [(Tabuleiro,Tempo,Carro)]
testesT3 = [
             ([[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],[Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]], 1.1 , Carro {posicao = (2.5 , 1.5), direcao = 0, velocidade = (1, 0)}),
             ([[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],[Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]], 0.9 , Carro {posicao = (2.5 , 1.5), direcao = 0, velocidade = (1, 0)}),
             ([[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],[Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]], 2   , Carro {posicao = (1.6 , 1.6), direcao = 0, velocidade = (0, 1)}),
             ([[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],[Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]], 2.5 , Carro {posicao = (1.6 , 1.6), direcao = 0, velocidade = (0, 1)})
           ]
-- * Movimentação do Carro
{- | Dado um Tabuleiro, um Tempo e um Carro, calcula o novo estado do carro após se ter movimentado.

Exemplo de Utilização:
>>> movimenta [[Peca Lava 0, Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca Recta 0, Peca (Curva Este) 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0]] 1.0 Carro {posicao = (0.6 , 0.4), direcao = 45, velocidade = (1, -2)}
Nothing

>>> movimenta [[Peca Lava 0, Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca Recta 0, Peca (Curva Este) 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0]] 1.0 Carro {posicao = (0.6 , 0.4), direcao = 45, velocidade = (0.7, 0.9)}
Just (Carro {posicao = (1.2999999999999998,1.3), direcao = 45.0, velocidade = (0.7,0.9)})

-}
movimenta :: Tabuleiro -> Tempo -> Carro -> Maybe Carro
movimenta tab t c = isDestroyed tab (move t c)

-- ** Movimentação do Carro sem Colisões nem Destruições
{-| Função que recebe um Carro e, asssumindo que este nem colide nem é destruído, devolve o carro ao fim do tempo dado.

Exemplo de Utilização:
>>> move 1 Carro {posicao = (1, 1), direcao = 45, velocidade = (2, 1)}
Carro {posicao = (3.0,2.0), direcao = 45.0, velocidade = (2.0,1.0)}

-}
move:: Tempo -> Carro -> Carro
move t Carro {posicao = (x,y), direcao = d, velocidade = (vx, vy)} = Carro {posicao = (x + vx * t ,y + vy * t), direcao = d, velocidade = (vx, vy)}

-- *** Destruição do Carro
{- | Função que verifica se o carro é destruído nas coordenadas onde se encontra.

== Exemplo de Utilização:
>>> isDestroyed [[Peca Lava 0, Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca Recta 0, Peca (Curva Este) 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0]] Carro {posicao = (0.6 , 0.4), direcao = 45, velocidade = (0.7, 0.9)}
Nothing

>>> isDestroyed [[Peca Lava 0, Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca Recta 0, Peca (Curva Este) 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0]] Carro {posicao = (1.6 , 1.4), direcao = 45, velocidade = (0.1,0.1)}
Just (Carro {posicao = (1.6,1.4), direcao = 45.0, velocidade = (0.1,0.1)})

-}
isDestroyed:: Tabuleiro -> Carro -> Maybe Carro
isDestroyed t c | peca == Peca Lava 0 = Nothing
                | isCurva peca && altPeca >= 0 = isDestroyedCurva t c 
                | otherwise = Just c
            where
                peca = pecaTabFinder t c
                altPeca = alturaPeca peca

-- **** Destruição do Carro nas Curvas
{- | Função que recebe um Tabuleiro e um Carro, devolvendo o estado do Carro. Assume que a Peca onde o Carro se encontra é uma Curva com altura igual ou superior a 0.

== Exemplo de Utilização:
>>> isDestroyedCurva [[Peca Lava 0, Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca Recta 0, Peca (Curva Este) 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0]] Carro {posicao = (2.6 , 1.4), direcao = 45, velocidade = (0.1,0.1)}
Nothing

>>> isDestroyedCurva [[Peca Lava 0, Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca Recta 0, Peca (Curva Este) 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0]] Carro {posicao = (2.0 , 1.2), direcao = 45, velocidade = (0.1,0.1)}
Just (Carro {posicao = (2.0,1.2), direcao = 45.0, velocidade = (0.1,0.1)})

-}
isDestroyedCurva:: Tabuleiro -> Carro -> Maybe Carro
isDestroyedCurva t c | peca == Curva Norte && ((-1 * xp) + 1) > yp
                    || peca == Curva Sul   && ((-1 * xp) + 1) < yp
                    || peca == Curva Este  && xp > yp
                    || peca == Curva Oeste && xp < yp = Nothing
                     | otherwise = Just c
            where
                peca = typePeca (pecaTabFinder t c)
                xp = fst (coorInPeca c)
                yp = snd (coorInPeca c)

-- ***** Orientação do Carro
{- | Função que dado um Carro, devolve a Orientação com que este sai da Peca onde se encontra.

== Exemplo de utilização:
>>>orExitCarro (Carro {posicao = (1.5,2.5), direcao = 45, velocidade = (1, 0)})
Este

-}
orExitCarro:: Carro -> Orientacao
orExitCarro c | xc >= xp     && yc == yp     = Norte
              | xc >= xp     && yc == yp + 1 = Sul
              | xc == xp + 1 && yc >= yp     = Este
              | xc == xp     && yc >= yp     = Oeste
            where
                (xc,yc) = colideCarro c
                (vx,vy) = coorPeca c
                (xp,yp) = (fromIntegral vx,fromIntegral vy)

-- ***** Coordenadas de Interseção Carro - Borda da Peca 

{- | Função que calcula as coordenadas do carro quando interseta a borda da Peca.

== Exemplo de utilização:
>>>colideCarro (Carro {posicao = (1.5,2.5), direcao = 45, velocidade = (1, 0)})
(2,2.5)

-}
colideCarro:: Carro -> Ponto
colideCarro c@ Carro {posicao = p, direcao = d, velocidade = v} = closestColide posList p

            where
                
                retas = probablyLines c
                posList = map (intersectRetas (p,v)) retas

-- ***** Ponto Provável de Colisão
{- | Função que através de dois pontos, retorno um outro Ponto.

== Exemplo de utilização:
>>> closestColide [Just (1.0,2.0), Just (3.1,2.9)] (2.4,2.4) 
(3.1,2.9)

>>> closestColide [Just (1.0,2.0), Nothing] (2.4,2.4) 
(1.0,2.0)

-}
closestColide :: [Maybe Ponto] -> Ponto -> Ponto
closestColide [Just x]          _ = x
closestColide [Just x, Nothing] _ = x
closestColide [Nothing, Just x] _ = x
closestColide [Just x, Just y]  p | d1 <= d2 = x
                                  | d1 >  d2 = y
                            
                            where
                                   d1 = distPonto x p
                                   d2 = distPonto y p 

-- ***** Peca - Carro  
{- | Função que recebe um Tabuleiro e um Carro e devolve a Peca sobre a qual o carro se encontra.

== Exemplo de utilização:
>>>pecaTabFinder [[Peca Lava 0]] (Carro {posicao = (0.5,0.5), direcao = 45, velocidade = (1, 1)})
Peca Lava 0

-}
pecaTabFinder:: Tabuleiro -> Carro -> Peca
pecaTabFinder t Carro {posicao = (x,y), direcao = d, velocidade = (vx, vy)} = (t !! yp) !! xp
            where
                xp = floor x
                yp = floor y



-- ***** Coordenadas Peca - Carro
{- | Função que recebe um Carro e devolve a Posicao da Peca onde este se encontra.

== Exemplo de utilização:
>>>coorPeca (Carro {posicao = (1.5,2.5), direcao = 45, velocidade = (1, 1)})
(1,2)

-}
coorPeca:: Carro -> Posicao
coorPeca Carro {posicao = (x,y), direcao = d, velocidade = (vx, vy)} = (xp, yp)
            where
                 xp = floor x
                 yp = floor y

-- ***** Coordenadas Carro
{- | Função que recebe um Carro e devolve as suas coordenadas, tendo em conta como origem do referencial o canto superior esquerdo da Peca onde se encontra.

== Exemplo de utilização:
>>>coorInPeca (Carro {posicao = (1.5,2.5), direcao = 45, velocidade = (1, 1)})
(0.5,0.5)

-}
coorInPeca:: Carro -> Ponto
coorInPeca c@ Carro {posicao = (x,y), direcao = d, velocidade = (vx, vy)} = (x - xp, y - yp)
            where
                xp = fromIntegral (fst (coorPeca c))
                yp = fromIntegral (snd (coorPeca c))

--  ***** Retas Limites da Peças         
{- | Função que recebendo um carro devolve os Vetores limites da Peca onde o Carro se encontra com as quais ele se pode intersetar.

== Exemplo de Utilização:
>>> probablyLines Carro {posicao = (0.6 , 0.4), direcao = 45, velocidade = (0.7, 0.9)}
[((1.0,1.0),(1.0,0.0)),((1.0,1.0),(0.0,1.0))]

-}
probablyLines :: Carro -> [RetaV]
probablyLines c@ Carro {posicao = (x,y), direcao = d, velocidade = (vx, vy)}   | vx >  0 && vy > 0 = [rSul   ,rEste ]
                                                                               | vx >  0 && vy < 0 = [rNorte ,rEste ]
                                                                               | vx <  0 && vy > 0 = [rSul   ,rOeste]
                                                                               | vx <  0 && vy < 0 = [rNorte ,rOeste]
                                                                               | vx == 0 && vy > 0 = [rSul  ]
                                                                               | vx == 0 && vy < 0 = [rNorte]
                                                                               | vy == 0 && vx > 0 = [rEste ]
                                                                               | vy == 0 && vx < 0 = [rOeste]
                                 where
                                        (px,py) = coorPeca c
                                        rNorte  = convert ((px  ,py  ) ,(1,0))
                                        rSul    = convert ((px+1,py+1) ,(1,0))
                                        rEste   = convert ((px+1,py+1) ,(0,1))
                                        rOeste  = convert ((px  ,py  ) ,(0,1))

-- ***** Interseção entre Retas
{- | Função que interseta duas retas definidas vetorialmente.

== Exemplo de utilização:
>>> intersectRetas ((0,0),(1,0)) ((0,0),(0,1))
Just (0,0)

>>> intersectRetas ((0,0),(1,0)) ((1,0),(1,0))
Nothing

-}
intersectRetas :: RetaV -> RetaV -> Maybe Ponto
intersectRetas ((px1,py1),(vx1,vy1)) ((px2,py2),(vx2,vy2)) = intersectionTwoPoints ((px1 , py1) , (px1 + vx1 , py1 + vy1)) ((px2,py2) , (px2 + vx2 , py2 + vy2))


-- ****** Interseção de Dois Pontos
{- | Recebendo um par de dois pontos que formam cada um deles uma reta, a função calcula o ponto de interseção das retas.

== Exemplo de Utilização:
>>> intersectionTwoPoints ((2.4,4.1),(2.1,2.2)) ((3.9,3.5),(4.2,3.7))

-}
intersectionTwoPoints :: ParPontos -> ParPontos -> Maybe Ponto
intersectionTwoPoints ( (ax , ay), (bx , by) ) ( (px , py) , (qx , qy) ) | determinante == 0 = Nothing
                                                                         | otherwise        = Just (f (px - qx) (ax - bx) , f (py - qy) (ay - by))
            where
                determinante = (ax - bx) * (py - qy) - (ay - by) * (px - qx)
                f pq ab      = ( (((ax * by) - (ay * bx)) * pq) - ((px * qy) - (py * qx)) * ab ) / determinante

-- ****** Funções Auxiliares
-- ******* Conversão de Tuplos para Retas
{- | Dado um Tuplo de pares com elementos do tipo Int e do tipo Double, converte-os para o tipo Reta V.

== Exemplo de Utilização:
>>>convert ((1,2),(2.1,2.2))
((1.0,2.0),(2.1,2.2))

-}
convert :: ((Int,Int),(Double,Double)) -> RetaV
convert ((x,y),v) = ((fromIntegral x,fromIntegral y), v)

-- ******* Distância entre Pontos
{- | Função que calcula a distância entre dois pontos.

== Exemplo de Utilização:
>>> distPonto (1.0,2.3) (2.1,3.2)
1.42126704035519

-}
distPonto:: Ponto -> Ponto -> Double
distPonto (ax,ay) (bx, by) = sqrt(((ax - bx)^2) + ((ay - by)^2))

-- ******* Identificação de Curvas
{- | Função que dada uma Peca, verifica se ela é do tipo Curva Orientacao.

== Exemplo de Utilização:
>>> isCurva (Peca (Curva Este) 0) 
True

>>> isCurva (Peca (Recta) 0) 
False

-}
isCurva:: Peca -> Bool
isCurva p =  typePeca p == Curva Norte
          || typePeca p == Curva Sul
          || typePeca p == Curva Este
          || typePeca p == Curva Oeste

-- ******* Tipos de Peças
{- | Função que recebe uma Peca e devolve o seu Tipo.

== Exemplo de utilização:
>>>typePeca (Peca Lava 0)
Lava

-}
typePeca :: Peca -> Tipo
typePeca (Peca x _) = x

-- ******* Verificação de Inteiros
{- | Função que verifica se o Double dado possui apenas zeros na parte decimal.

== Exemplo de Utilização:
>>> isInt 2.8
False

>>> isInt 2.0
True

-}
isInt :: Double -> Bool
isInt x = x - fromIntegral (floor x) == 0 


{- | Função que recebe uma Peca e devolve a sua Altura.

== Exemplo de utilização:
>>>alturaPeca (Peca Recta 1)
1

-} 
alturaPeca :: Peca -> Int
alturaPeca (Peca _ x) = x

-- ******* Altura da Peça
{- | Função que dada uma Peca, devolve o valor da sua Altura.
Exemplo de utilização:

>>> find_high (Peca Lava 1)
1

-}
positionCarro :: Carro -> Ponto
positionCarro Carro {posicao = (x,y), direcao = d, velocidade = (vx, vy)} = (x,y)


-- ** Funções a Desenvolver
-- *** Colisões

-- **** Identificação de Colisão
{- | Função que recebe um Carro, a Altura da Peca em que se encontra e a Peca para onde o Carro irá transitar. Devolve um valor Booleano. Ele diz se irá existir ricochete na passagem do Carro para a Peca dada.
== Exemplo da utilização:

>>> isRicochete (Carro {posicao = (2.3,3.4), direcao = 45, velocidade = (4.0,5.2)}) (-1) (Peca (Curva Este) 0)
True

-}

isRicochete :: Carro -> Altura -> Peca -> Bool
isRicochete c@ Carro {posicao = (x,y), direcao = d , velocidade = (vx, vy)} h p = h < alturaPeca p  

-- **** Vetor Unitário - Colisão fora de uma Curva
{- | Função que dá as coordenadas do vetor normal da reta perpendicular ao ponto de colisão de um Carro numa Peca que não seja do Tipo Curva.
     Recebe as coordenadas do vetor diretor da reta onde se encontra o ponto de colisão.

== Exemplo de Utilização:
>>> normalVector (2.0,4.2)(2.1,2.3)
(-1.0,0.0)

-} 
unitaryVector ::  Ponto -> Velocidade -> Vetor
unitaryVector (x , y) (vx , vy)  | verticalCrash    && (vx > 0 && vy > 0) = (-1.0 , 0.0)
                                 | verticalCrash    && (vx > 0 && vy < 0) = (-1.0 , 0.0)
                                 | verticalCrash    && (vx < 0 && vy < 0) = ( 1.0 , 0.0)
                                 | verticalCrash    && (vx < 0 && vy > 0) = ( 1.0 , 0.0)
                                 | horizontalCrash  && (vx > 0 && vy > 0) = ( 0.0 , 1.0)
                                 | horizontalCrash  && (vx < 0 && vy > 0) = ( 0.0 , 1.0)
                                 | horizontalCrash  && (vx < 0 && vy < 0) = ( 0.0 ,-1.0)
                                 | horizontalCrash  && (vx > 0 && vy < 0) = ( 0.0 ,-1.0)
                               
                       
                       where 
                             verticalCrash   = isInt x 
                             horizontalCrash = isInt y

-- **** Vetor Unitário - Colisão numa Curva
{- | Função que dá as coordenadas do vetor normal da reta perpendicular ao ponto de colisão de um Carro numa Peca que seja do Tipo Curva.
     Recebe as coordenadas do vetor diretor da reta onde se encontra o ponto de colisão.
   
== Exemplo de Utilização:
>>> unitaryVectorC (1.5,1.5) (-3.0,4.0)
(1.0,1.0)

-}
unitaryVectorC ::  Ponto -> Velocidade -> Vetor
unitaryVectorC (x , y) (vx , vy) | curvaEste  (x , y) && vx > 0 && vy > 0 = (1.0 , 1.0)
                                 | curvaEste  (x , y) && vx < 0 && vy > 0 = (1.0 , 1.0)
                                 | curvaOeste (x , y) && vx > 0 && vy < 0 = (1.0 ,-1.0)
                                 | curvaOeste (x , y) && vx > 0 && vy > 0 = (1.0 ,-1.0)

                      where 
                            curvaEste  :: Ponto -> Bool
                            curvaEste  (x , y) = (y / x) > 0
                            
                            curvaOeste :: Ponto -> Bool
                            curvaOeste (x , y) = (y / x) < 0

-- **** Produto Interno
{- | Função que dados dois pontos, calcula o resultado do seu produto interno. Este é dado sob a forma de Double.

== Exemplo de Utilização:
>>> internalProduct (1.5,1.5) (-3.0,4.0)
1.5

-}
internalProduct :: Ponto -> Ponto -> Double
internalProduct (x , y) (a , b) =  x * a + y * b 

-- **** Velocidade após Ricochete - Não Curva
-- | Função que dado um Carro e um Tempo, devolve a Velocidade após uma colisão. Esta colisão não ocorre numa Curva.

velRicocheteNC :: Carro -> Tempo -> Velocidade
velRicocheteNC c@ Carro {posicao = (x , y), direcao = d , velocidade = (vx , vy)} t  = (vx - 2 * p * xn , vy - 2 * p * yn)  
                                                
                                         where n = unitaryVector (posRicochete ((x , y) , (vx , vy)) t) (vx, vy)
                                               (xn, yn) = n
                                               p  = internalProduct (x, y) n


-- **** Velocidade após Ricochete - Curva
-- | Função que dado um Carro e um Tempo, devolve a Velocidade após uma colisão. Esta colisão ocorre numa Curva.

velRicocheteC :: Carro -> Tempo -> Velocidade
velRicocheteC c@ Carro {posicao = (x , y), direcao = d , velocidade = (vx , vy)} t  = (vx - 2 * p * xn , vy - 2 * p * yn)  
                                                
                                         where n  = unitaryVectorC (posRicochete ((x , y) , (vx , vy)) t) (vx, vy)
                                               xn = fst n
                                               yn = snd n
                                               p  = internalProduct (x, y) n
-- **** Posição do Ricochete
{- | Função que recebe uma RetaV e um valor do Tempo, devolvendo o ponto no Mapa onde o vetor Velocidade se irá se situar após uma colisão.

== Exemplo da utilização:
>>> posRicochete ((0,0),(1,0)) 2.0 
(2.0,0.0)

-}
posRicochete :: RetaV -> Tempo -> Ponto
posRicochete ((x , y) , (vx , vy)) t  = (x + vx * t, y + vy * t)

{- | Função que dadas as Coordenadas da Velocidade, calcula o seu módulo.

== Exemplo da utilização:
>>> velocityModule (2.1,2.2)
3.0413812651491097

-}
velocityModule :: Velocidade -> Double
velocityModule (vx, vy) = sqrt (vx^2 + vy^2)

-- **** Tempo Carro - Colisão
{- | Dados um ParPontos e um Double, relativo ao módulo da Velocidade, a função devolve o Tempo que demore a ir de para o outro.

-- | Função que recebe um ParPontos e um Double, correspondente ao valor modular da Velocidade. Devolve o Tempo que o Carro demora a percorrer distância entre o par de pontos.

== Exemplo de Utilização:
>>> timeToCrash ((1.1,2.1),(3.2,3.2)) 0.7
3.386648454608485

-}
timeToCrash :: ParPontos -> Double -> Tempo
timeToCrash ((x,y), (a , b)) v = distPonto (x,y) (a , b) / v                                  










