{-|
Module      : Tarefa4_2017li1g13
Description : Módulo da Tarefa 4 para LI1 17/18

Módulo para a realização da Tarefa 4 de LI1 em 2017/18.
-}
module Tarefa4_2017li1g13 where


import LI11718
import Tarefa2_2017li1g13
import Tarefa3_2017li1g13
import Data.Maybe

-- | Vetor definido como um par que contém o seu angulo em graus e a sua norma.
type VetorAng = (Double, Double)
{-|
O testes a serem considerados pelo sistema de /feedback/
para a função 'atualiza'.
-}
-- | Conjunto de testes para a Tarefa 4
testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = [(1  , j1 ,Acao {acelerar = True , travar = False, esquerda = False, direita = False, nitro = Nothing}),
            (1  , j1 ,Acao {acelerar = False, travar = True , esquerda = False, direita = False, nitro = Nothing}),
            (0.5, j1 ,Acao {acelerar = True , travar = False, esquerda = True , direita = False, nitro = Nothing}),
            (0.5, j1 ,Acao {acelerar = True , travar = False, esquerda = False, direita = True , nitro = Nothing}),
            (1  , j1 ,Acao {acelerar = True , travar = False, esquerda = False, direita = False, nitro = Just 0} ),
            (1  , j1 ,Acao {acelerar = True , travar = False, esquerda = True , direita = True , nitro = Nothing}),
            (1  , j2 ,Acao {acelerar = True , travar = False, esquerda = False, direita = False, nitro = Nothing}),
            (1  , j2 ,Acao {acelerar = False, travar = True , esquerda = False, direita = False, nitro = Nothing}),
            (0.5, j2 ,Acao {acelerar = True , travar = False, esquerda = True , direita = False, nitro = Nothing}),
            (0.5, j2 ,Acao {acelerar = True , travar = False, esquerda = False, direita = True , nitro = Nothing}),
            (1  , j2 ,Acao {acelerar = True , travar = False, esquerda = False, direita = False, nitro = Just 0} ),
            (1  , j2 ,Acao {acelerar = True , travar = False, esquerda = True , direita = True , nitro = Nothing}),
            (1  , j3 ,Acao {acelerar = True , travar = False, esquerda = False, direita = False, nitro = Nothing}),
            (1  , j3 ,Acao {acelerar = False, travar = True , esquerda = False, direita = False, nitro = Nothing}),
            (0.5, j3 ,Acao {acelerar = True , travar = False, esquerda = True , direita = False, nitro = Nothing}),
            (0.5, j3 ,Acao {acelerar = True , travar = False, esquerda = False, direita = True , nitro = Nothing}),
            (1  , j3 ,Acao {acelerar = True , travar = False, esquerda = False, direita = False, nitro = Just 0} ),
            (1  , j3 ,Acao {acelerar = True , travar = False, esquerda = True , direita = True , nitro = Nothing}),
            (1  , j4 ,Acao {acelerar = True , travar = False, esquerda = False, direita = False, nitro = Nothing}),
            (1  , j4 ,Acao {acelerar = False, travar = True , esquerda = False, direita = False, nitro = Nothing}),
            (0.5, j4 ,Acao {acelerar = True , travar = False, esquerda = True , direita = False, nitro = Nothing}),
            (0.5, j4 ,Acao {acelerar = True , travar = False, esquerda = False, direita = True , nitro = Nothing}),
            (1  , j4 ,Acao {acelerar = True , travar = False, esquerda = False, direita = False, nitro = Just 0} ),
            (1  , j4 ,Acao {acelerar = True , travar = False, esquerda = True , direita = True , nitro = Nothing})
           ]

-- | 'Jogo' utilizado nos primeiros três testes dos 'testesT4'
j1 :: Jogo
j1 = Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]], pista =  prop, carros =  [Carro {posicao = (2.5,1.5), direcao = 0, velocidade = (1, 0)}], nitros = [5.0], historico =  [[(2,1)]]}

-- | 'Jogo' utilizado nos últimos três testes dos 'testesT4'
j2 :: Jogo
j2 = Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]], pista =  prop, carros =  [Carro {posicao = (1.5,2.5), direcao = 270 , velocidade = (0, 1)}], nitros = [5.0], historico =  [[(2,1)]]}

-- | União do 'j1' e 'j2'
j3 :: Jogo
j3 = Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]], pista =  prop, carros =  [Carro {posicao = (1.5,2.5), direcao = 270 , velocidade = (0, 1)},Carro {posicao = (2.5,1.5), direcao = 0, velocidade = (1, 0)}], nitros = [5.0,5.0], historico =  [[(2,1)],[(2,1)]]}

-- | 'Jogo' que testa o atrito dos carros 
j4 :: Jogo
j4 = Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]], pista =  prop, carros =  [Carro {posicao = (1.5,2.5), direcao = 0 , velocidade = (0, 1)},Carro {posicao = (2.5,1.5), direcao = 90, velocidade = (1, 0)}], nitros = [5.0,5.0], historico =  [[(2,1)],[(2,1)]]}

-- | 'Propriedades' utilizadas no 'Jogo' 'j1', 'j2' e 'j3'
prop :: Propriedades
prop = Propriedades 2 3 4 2 15 180

-- | Função usada para atualizar o estado do 'Jogo' dada a 'Acao' de um jogador num determinado período de 'Tempo'.
atualiza :: Tempo -- ^ a duração da ação
         -> Jogo  -- ^ o estado do jogo
         -> Int   -- ^ o identificador do jogador
         -> Acao  -- ^ a ação tomada pelo jogador
         -> Jogo  -- ^ o estado atualizado do jogo
atualiza t e j a = atCarros t atHistNit j a
                where
                    carroHistNew = atHist carroTarget carroHistOld
                    carroTarget  = carros e !! j
                    carroHistOld = jogoHistOld !! j
                    atHistNit    = e {nitros = tNitNew, historico = jogoHistNew}
                    tNitNew      = tNitDiscount t e j a
                    jogoHistNew  = atList jogoHistOld carroHistNew j
                    jogoHistOld  = historico e


-- | Função que desconta o 'Tempo' do Nitro no registo de Nitros e devolve um novo registo de Nitros
tNitDiscount:: Tempo -> Jogo -> Int -> Acao -> [Tempo]
tNitDiscount t j i a | isNit     = atList listTNits tNitFinal i
                     | otherwise = listTNits         
            where
                isNit     = isJust (nitro a)
                tNitCar   = listTNits !! i
                listTNits = nitros j
                tNitFinal = aux (tNitCar - t)
                            where
                                aux:: Double -> Double
                                aux t | t < 0     = 0
                                      | otherwise = t


{- | Função que recebe um 'Carro' e o histórico deste e atualiza-o

obs: note que o histórico é lido da direita para a esquerda, ou seja, a 'Posicao' mais recente encontra-se à esquerda e a mais antiga à direita

==Exemplo de Utilização:
>>> atHist Carro {posicao = (1.5,2.5), direcao = 270 , velocidade = (0, 1)} [(1,1)]
[(1,2),(1,1)]
-}
atHist:: Carro -> [Posicao] -> [Posicao]
atHist c log | head log == coorCarro   = log
             | otherwise               = coorCarro : log
             where
                 coorCarro = coorPeca c
-- * Atualizar Carros
-- | Função que atualiza o estado dos Carros
atCarros::Tempo -> Jogo -> Int -> Acao -> Jogo
atCarros t j i a = j {carros = applyNit t carNotNit tNit prop i a}
            where
                prop      = pista j
                car       = carros j !! i
                v         = velocidade car
                dCha      = dirCarChange t j i a
                vRes      = vetorRes  t j i a
                tNit      = nitros j
                carNotNit = atList (carros j) (car {direcao = dCha, velocidade = vRes}) i

-- ** Atualizar Direcao
-- | Função que dado um Tempo, um Jogo, um identificativo do jogador e uma Acao, devolve a nova direcao do jogador
dirCarChange::Tempo -> Jogo -> Int -> Acao -> Double
dirCarChange t j i a | esq && dir = dirCar
                     | esq        = dirCar + deltaD
                     | dir        = dirCar - deltaD
                     | otherwise  = dirCar
            where
                dirCar = direcao (carros j !! i)
                esq    = esquerda a
                dir    = direita a
                prop   = pista j 
                deltaD = t * k_roda prop

-- ** Atualizar Nitro
-- | Função que aplica sobre um jogo o efeito do nitro ao longo de um intervalo de 'Tempo' e devolve uma lista de carros com essa mudança. 
applyNit::Tempo -> [Carro] -> [Tempo] -> Propriedades -> Int -> Acao -> [Carro]
applyNit t cars tNit prop i a | isNothing nitID = cars
                              | otherwise        = atList cars (car {velocidade = (vx + nx, vy + ny)}) (fromJust nitID)
            where
                nitID    = nitro a
                car      = cars !! fromJust nitID
                (vx, vy) = velocidade car
                (nx, ny) = nit ((tNit !! i) > 0) t car prop

-- | Função que devolve o 'Vetor' velocidade correspondente ao atuar do nitro ao longo de um intervalo dado. 
nit::Bool -> Tempo -> Carro -> Propriedades -> Vetor
nit b t car prop | b         = vetAngToCoor (direcao car, t * k_nitro prop)
                 | otherwise = (0,0)


-- ** Cálculo do Vetor resultante
-- | Funçao que, utilizando a função 'vetoresRes', devolve o 'Vetor' velocidade que atua sobre um determinado carro 
vetorRes:: Tempo -> Jogo -> Int -> Acao -> Vetor
vetorRes t j i a = sumVet (vetoresRes t j i a)

-- *** Calculo dos diversos vetores
-- | Função que calcula todos os 'Vetor' velocidade que atuam sobre um determinado carro, exceto o nitro.
vetoresRes::Tempo -> Jogo -> Int -> Acao -> [Vetor]
vetoresRes t j i a = [(vx,vy),
                      acel (acelerar a) t car prop,
                      trav (travar a)   t car prop,
                      peso isPeso t car prop anVetPeso,
                      pneu t car prop,
                      atrito]
             where
                m         = mapa j
                prop      = pista j
                car       = carros j !! i
                (vx,vy)   = velocidade car
                (va, vn)  = vetCoorToAng (velocidade car)
                pecaLocal = pecaFinder m (coorPeca car)
                isPeso    = tipoFinder pecaLocal == Rampa Norte
                atrito    = vetAngToCoor (va - 180, t * vn * k_atrito prop)
                anVetPeso = orToAng (orFinder pecaLocal Norte) - 180
                
-- **** Cálculo do vetor Aceleração
-- | Função que devolve o 'Vetor' velocidade correspondente ao atuar da aceleração ao longo de um intervalo de 'Tempo' dado. 
acel::Bool -> Tempo -> Carro -> Propriedades -> Vetor  
acel b t car prop | b         = vetAngToCoor (direcao car, t * k_acel prop)
                  | otherwise = (0,0)

-- **** Cálculo do vetor Travagem
-- | Função que devolve o 'Vetor' velocidade correspondente ao atuar da travagem ao longo de um intervalo de 'Tempo' dado. 
trav::Bool -> Tempo -> Carro -> Propriedades -> Vetor
trav b t car prop | b         = vetAngToCoor (direcao car + 180, t * k_acel prop)
                  | otherwise = (0,0)

-- **** Cálculo do vetor Peso
-- | Função que devolve o 'Vetor' velocidade correspondente ao atuar do peso ao longo de um intervalo de 'Tempo' dado. 
peso::Bool -> Tempo -> Carro -> Propriedades -> Double -> Vetor
peso b t car prop a| b         = vetAngToCoor (a, t * k_peso prop)
                   | otherwise = (0,0)

-- **** Cálculo do vetor Pneu
-- | Função que calcula o 'Vetor' velocidade correspondente ao atuar dos Pneus ao longo de um intervalo de 'Tempo' dado. 
pneu::Tempo -> Carro -> Propriedades -> Vetor
pneu t car prop | angCarro <= angVel = vetAngToCoor ( angCarro - 90, t * normaPneus * normVel)
                | angCarro >  angVel = vetAngToCoor ( angCarro + 90, t * normaPneus * normVel)
            where
                angCarro         = direcao car
                (angVel,normVel) = vetCoorToAng (velocidade car)
                normaPneus       = sin (grauToRad(abs (angCarro - angVel))) * k_pneus prop 


-- * Funções auxiliares
-- ** Sobre Vetores
-- *** Conversão de Vetores

{- | Função que converte 'VetorAng' em 'Vetor'.

== Exemplo de Utilização:
>>> vetCoorToAng  (0,2)
(0,2)

>>> vetCoorToAng (pi/2,1)
(1,1)
-}
vetAngToCoor:: VetorAng -> Vetor
vetAngToCoor (a,n) = (x, y)
            where
                x = n * cos (grauToRad a)
                y = - n * sin (grauToRad a)

{- | Função que converte 'Vetor' em 'VetorAng'. 

== Exemplo de Utilização:
>>> vetCoorToAng  (-2,1)
(206.56505117707798,2.23606797749979)


>>> vetCoorToAng (2,1)
(333.434948822922,2.23606797749979)

-}
vetCoorToAng:: Vetor -> VetorAng
vetCoorToAng (x,y) = (angVet (x,y), n)
            where
                n = sqrt((x^2) + (y^2))

-- *** Angulo de Vetores
{- | Função que calcula o angulo de um 'Vetor' em graus.

==Exemplo de Utilização:
>>>angVet (1,1)
315.0

>>>angVet (-1.8,-2.4)
126.86989764584402
-}
angVet:: Vetor -> Double
angVet (x,y) | x == 0 && y == 0 = 0
             | y < 0            = radToGrau (acos v)
             | otherwise        = radToGrau (acos v + 2 * acos (-v))
            where
                v = x / sqrt (x*x + y*y)

-- *** Listas de Vetores

{- | Função que dada uma lista de 'Vetor', devolve o 'Vetor' resultante da soma de todos eles.
== Exemplo de Utilização:
>>> sumVet [(1.5,1.5),(2.0,4.0)]
(3.5,5.5)

>>> sumVet [(-1.0,-1.5),(-2.0,-5.0),(3.0,6.0),(8.0,-2.5)]
(8.0,-3.0)

-}
sumVet :: [Vetor] -> Vetor
sumVet [x] = x
sumVet ((x,y):xs) = (x + rx, y + ry)
            where
                (rx, ry) = sumVet xs



-- ** Sobre Angulos
{- | Função que converte um angulo em radianos para graus.
== Exemplo de Utilização:
>>> radToGrau pi/6
30.0

>>> radToGrau (-3*pi)/5
-108.0

-}
radToGrau:: Double -> Double
radToGrau x = x * 180 / pi

{- | Função que converte um ângulo em graus para um ângulo em radianos.

== Exemplo de Utilização:
>>> grauToRad (-8.9)
-0.15533430342749532

>>> grauToRad 135
2.356194490192345
 
-}
grauToRad:: Double -> Double
grauToRad x = x * pi / 180




{-| Função que converte uma 'Orientacao' para um 'Angulo'

==Exemplo de Utilização:
>>>orToAng Norte
90

>>>orToAng Sul
270
-}
orToAng:: Orientacao -> Double
orToAng Norte = 90 
orToAng Sul   = 270
orToAng Este  = 0
orToAng Oeste = 180

-- ** Funções genéricas
{-| Função que recebe uma lista e substitui na posição dada o elemento da lista por outro

==Exemplo de Utilização:
>>> atList [1,2,3,4] 2 0
[2,2,3,4]

>>> atList [1,2,3,4] 2 3
[1,2,3,2]
-}
atList:: [a] -> a -> Int -> [a]
atList l e p = take p l ++ [e] ++ drop (p + 1) l