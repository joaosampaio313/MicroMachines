{-|
Module      : Tarefa1_2017li1g13
Description : Tarefa 1 do projeto a desenvolver no âmbito da disciplina de LI1.
Copyright   : João Sampaio <a85318@uminho.pt>; José Ferreira <a83683@uminho.pt>

Tarefa 1 do projeto a desenvolver no âmbito da disciplina de LI1.
Contém a função constroi que devolve um Mapa, com base num caminho dado.
-}

module Tarefa1_2017li1g13  where

import LI11718


-- | Responsável pelos testes da Tarefa 1

testesT1 :: [Caminho]
testesT1 = [ 
            [CurvaDir,Avanca,Avanca,CurvaDir,CurvaDir,CurvaEsq,CurvaDir,Avanca,Sobe,Sobe,Desce],[CurvaDir,Sobe,Avanca,Avanca,Desce],[CurvaEsq,CurvaDir,CurvaDir,Avanca,Desce,Avanca],[Desce,Desce,Sobe,Sobe,Desce,CurvaEsq,CurvaEsq,CurvaEsq,CurvaDir],[CurvaDir,Desce,Desce,Sobe,Sobe,Avanca,Avanca,CurvaDir,CurvaEsq,CurvaDir],[  ],
            [Avanca,Desce,Sobe,Sobe,Sobe,Sobe,Sobe,Sobe,Sobe,Desce,CurvaEsq,CurvaEsq,CurvaEsq],[CurvaEsq,CurvaEsq,CurvaDir,CurvaDir,CurvaDir,CurvaDir,CurvaDir],[CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaDir],[Sobe,Sobe,Sobe,Sobe,Sobe,Sobe,Sobe,Desce,Desce,CurvaEsq,CurvaDir],[CurvaDir,CurvaEsq,Avanca,Sobe],
            [Desce,Avanca,Avanca,Avanca,Avanca,Sobe,Sobe,Sobe,CurvaEsq,CurvaEsq,CurvaDir,CurvaDir,CurvaDir,CurvaDir,CurvaDir,Desce,Sobe],[Avanca,Sobe,CurvaDir,CurvaEsq,CurvaEsq,CurvaDir,CurvaDir,Sobe,Desce,Desce,Sobe,CurvaEsq,CurvaEsq,Avanca,Desce,Sobe,Avanca],[CurvaDir,CurvaDir,CurvaDir,CurvaDir,CurvaDir,CurvaDir,CurvaEsq],
            [CurvaEsq,CurvaDir,Avanca,Desce,Desce,Avanca,Avanca],[CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq],[CurvaDir,CurvaDir,CurvaDir,CurvaDir,CurvaDir,CurvaDir],[CurvaEsq,Sobe,Sobe,Avanca,CurvaEsq,CurvaDir,CurvaDir,CurvaEsq,Desce,Desce,Avanca,Sobe],[Desce,Desce,Desce,Desce,Sobe,Avanca,Avanca,Avanca,Avanca],
            [Desce,CurvaDir,CurvaEsq,Sobe,Sobe,CurvaDir,CurvaDir,CurvaDir,CurvaDir,CurvaDir,CurvaEsq,Desce,Desce,Sobe,Avanca],[CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq],[CurvaEsq,Sobe,Sobe,Avanca,CurvaEsq,CurvaDir,CurvaDir,CurvaEsq,Desce,Desce,Avanca,Sobe],[CurvaEsq,CurvaDir,Avanca,Desce,Desce,Avanca,Avanca],
            [Avanca,CurvaDir,Sobe,CurvaDir,Avanca,CurvaDir,Desce,CurvaDir],[Avanca,Sobe,CurvaDir,CurvaEsq,CurvaDir,CurvaDir,Sobe,Desce],[CurvaEsq,CurvaDir,Avanca,Desce,Desce,Avanca,Avanca],[Avanca,CurvaDir,Sobe,CurvaDir,Avanca,CurvaDir,Desce,CurvaDir],[CurvaDir,CurvaDir,Desce,CurvaEsq,CurvaDir,CurvaDir,Sobe,Desce,CurvaDir],
            [Avanca, CurvaEsq, CurvaEsq, CurvaEsq,Avanca, CurvaDir, CurvaDir, Avanca, CurvaEsq, CurvaEsq, Avanca, CurvaDir, Avanca, CurvaEsq, CurvaEsq, Avanca, CurvaDir, CurvaDir,Avanca,CurvaEsq,CurvaEsq,Avanca,CurvaDir,Avanca,CurvaEsq,CurvaEsq, Avanca,CurvaDir,CurvaDir,Avanca,CurvaEsq,CurvaEsq,Avanca,CurvaDir,Avanca, CurvaEsq, CurvaEsq, Avanca,CurvaDir,CurvaDir,Avanca,CurvaEsq,Avanca,CurvaEsq,CurvaEsq,Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca]
           ]    

-- * Construção do Mapa
{- | Faz a construção de um Mapa a partir do Caminho dado.

== Exemplo de Utilização:
>>> constroi [CurvaDir,CurvaEsq,Avanca,Sobe]
Mapa ((1,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-}
constroi :: Caminho -> Mapa
constroi c = Mapa (partida c, Este) (makeTabuleiro c) 

-- ** Construção do Tabuleiro Final

{- | A construção de um Tabuleiro relaciona, essencialmente, 4 funções:

   * editTabuleiro
   
   * buildLava
   
   * passosToPecas
   
   * passosToPosition

    A função makeTabuleiro é onde ocorre a junção das funções referidas.
-}

{- | Recebe um Caminho e devolve um Tabuleiro com as Pecas necessárias.
 
== Exemplo de Utilização:
>>> makeTabuleiro [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe]
[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-}
makeTabuleiro :: Caminho -> Tabuleiro
makeTabuleiro c = editTabuleiro (buildLava c) (passosToPecas c) (passosToPosition c)

-- *** Construção do Tabuleiro de Peca Lava 0
    
{- | Devolve um Tabuleiro repleto de Peca Lava 0 em virtude da dimensão do Caminho que lhe é dado.

== Exemplo de utilização:
>>> buildLava [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe]
   [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

>>> buildLava []
  [[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-}
buildLava :: Caminho -> Tabuleiro
buildLava c = let x = fst (dimensao c)
                  y = snd (dimensao c)  
              in  replicate y (replicate x (Peca Lava 0))

-- *** Posições no Tabuleiro
    
{- | Começando na partida, fornece uma lista do tipo Posicao resultante de um Caminho dado. O primeiro Passo é feito para Este.

== Exemplo de Utilização:
>>> passosToPosition [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe]
[(1,5),(2,5),(3,5),(4,5),(4,4),(5,4),(5,3),(5,2),(5,1)]

-}
passosToPosition:: Caminho -> [Posicao]
passosToPosition [] = []
passosToPosition c = positionEste (partida c) c

-- **** Funções necessárias para a lista de Posições 
{-  As próximas 4 funções relacionam-se entre si na função passosToPosition.
    Permitem a construção de uma lista de Posicao através de um Caminho e uma Posicao inicial. Diferem na Orientação para o qual é dado o primeiro Passo.
-}

{- | Através de uma Posicao inicial, devolve uma lista do tipo Posicao referente ao Caminho dado. O primeiro Passo é feito para Norte.

== Exemplos de Utilização:
>>> positionNorte (2,1) [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe] 
[(2,1),(2,0),(2,-1),(2,-2),(1,-2),(1,-3),(0,-3),(-1,-3),(-2,-3)]

-}
positionNorte :: Posicao -> Caminho -> [Posicao]
positionNorte _ [] = []
positionNorte (x,y) (h:t) | h == Avanca   = (x , y) : positionNorte  (x , y - 1) t
                          | h == CurvaDir = (x , y) : positionEste   (x + 1 , y) t
                          | h == CurvaEsq = (x , y) : positionOeste  (x - 1 , y) t 
                          | h == Sobe     = (x , y) : positionNorte  (x , y - 1) t
                          | h == Desce    = (x , y) : positionNorte  (x , y - 1) t


{- | Através de uma Posicao inicial, devolve uma lista do tipo Posicao referente ao Caminho dado. O primeiro Passo é feito para Sul.

== Exemplo de Utilização
>>> positionSul (2,1) [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe] 
[(2,1),(2,2),(2,3),(2,4),(3,4),(3,5),(4,5),(5,5),(6,5)]

-}
positionSul :: Posicao -> Caminho -> [Posicao] 
positionSul _ [] = []
positionSul (x,y) (h:t)   | h == Avanca     = (x , y) : positionSul   (x , y + 1) t
                          | h == CurvaDir   = (x , y) : positionOeste (x - 1 , y) t
                          | h == CurvaEsq   = (x , y) : positionEste  (x + 1 , y) t
                          | h == Sobe       = (x , y) : positionSul   (x , y + 1) t
                          | h == Desce      = (x , y) : positionSul   (x , y + 1) t


{- | Através de uma Posicao inicial, devolve uma lista do tipo Posicao referente ao Caminho dado. O primeiro Passo é feito para Este.

== Exemplo de Utilização:
>>> positionEste (2,1) [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe] 
[(2,1),(3,1),(4,1),(5,1),(5,0),(6,0),(6,-1),(6,-2),(6,-3)]

-}
positionEste :: Posicao -> Caminho -> [Posicao] 
positionEste _ [] = []
positionEste (x,y) (h:t)  | h == Avanca   = (x , y) : positionEste   (x + 1 , y) t
                          | h == CurvaDir = (x , y) : positionSul    (x , y + 1) t
                          | h == CurvaEsq = (x , y) : positionNorte  (x , y - 1) t
                          | h == Sobe     = (x , y) : positionEste   (x + 1 , y) t
                          | h == Desce    = (x , y) : positionEste   (x + 1 , y) t


{- | Através de uma Posicao inicial, devolve uma lista do tipo Posicao referente ao Caminho dado. O primeiro Passo é feito para Oeste.
 
== Exemplo de Utilização:
>>> positionOeste (2,1) [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe] 
[(2,1),(1,1),(0,1),(-1,1),(-1,2),(-2,2),(-2,3),(-2,4),(-2,5)] 

-}
positionOeste :: Posicao -> Caminho -> [Posicao] 
positionOeste _ [] = []
positionOeste (x,y) (h:t) | h == Avanca   = (x , y) : positionOeste  (x - 1 , y) t
                          | h == CurvaDir = (x , y) : positionNorte  (x , y - 1) t
                          | h == CurvaEsq = (x , y) : positionSul    (x , y + 1) t 
                          | h == Sobe     = (x , y) : positionOeste  (x - 1 , y) t
                          | h == Desce    = (x , y) : positionOeste  (x - 1 , y) t 

-- *** Peças do Tabuleiro
{- | Permite a construção de uma lista com todas as Pecas necessárias, segundo um Caminho dado. 
     
      Tem em conta que:
      
      * O primeiro passo é feito para Este;
      
      * A Peca inicial tem Altura 0.  

== Exemplo de Utilização :
>>> passosToPecas [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe]  
[Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Curva Sul) 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca (Rampa Sul) (-1),Peca (Rampa Norte) (-1)]

-}
passosToPecas :: Caminho -> [Peca]
passosToPecas [] = []
passosToPecas c = pecasEste 0 c

-- **** Funções necessárias para a lista de Peças.

{-  As próximas 4 funções relacionam-se na função passosToPecas.
    Calculam uma lista de Pecas através de uma Altura inicial e de um Caminho. Diferem na Orientação em que é dado o primeiro Passo.
-}

{- | Recebe uma Altura inicial e um Caminho, devolvendo uma lista de Pecas necessárias para a sua realização. O primeiro Passo é feito para Este.

== Exemplos de Utilização:
>>> pecasEste 0 [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe] 
[Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Curva Sul) 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca (Rampa Sul) (-1),Peca (Rampa Norte) (-1)]

-}
pecasEste :: Altura -> Caminho -> [Peca]
pecasEste _ [] = [] 
pecasEste a (h:t)  | h == Avanca   = Peca Recta         (highCheck a Avanca)   : pecasEste  (highCheck a Avanca) t 
                   | h == CurvaDir = Peca (Curva Este)  (highCheck a CurvaDir) : pecasSul   (highCheck a CurvaDir) t 
                   | h == CurvaEsq = Peca (Curva Sul)   (highCheck a CurvaDir) : pecasNorte (highCheck a CurvaDir) t 
                   | h == Sobe     = Peca (Rampa Este)  (highCheck a Sobe)     : pecasEste  (highCheck (afterRise a Sobe) Sobe) t 
                   | h == Desce    = Peca (Rampa Oeste) (highCheck a Desce)    : pecasEste  (highCheck a Desce) t

{- | Recebe uma Altura inicial e um Caminho , devolvendo uma lista de Pecas necessárias para a sua realização. O primeiro Passo é feito para Oeste.    
 
== Exemplo de Utilização: 
>>> pecasOeste 0 [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe] 
[Peca Recta 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca (Curva Norte) 0,Peca (Rampa Norte) (-1),Peca (Rampa Sul) (-1)]

-}
pecasOeste :: Altura -> Caminho -> [Peca]
pecasOeste _ [] = [] 
pecasOeste a (h:t) | h == Avanca   = Peca Recta         (highCheck a Avanca)    : pecasOeste (highCheck a Avanca) t
                   | h == CurvaDir = Peca (Curva Oeste) (highCheck a CurvaDir)  : pecasNorte (highCheck a CurvaDir) t
                   | h == CurvaEsq = Peca (Curva Norte) (highCheck a CurvaEsq)  : pecasSul   (highCheck a CurvaEsq) t
                   | h == Sobe     = Peca (Rampa Oeste) (highCheck a Sobe)      : pecasOeste (highCheck (afterRise a Sobe) Sobe) t 
                   | h == Desce    = Peca (Rampa Este)  (highCheck a Desce)     : pecasOeste (highCheck a Desce) t


{- | Recebe uma Altura inicial e um Caminho , devolvendo uma lista de Pecas necessárias para a sua realização. O primeiro Passo é feito para Norte.    

== Exemplo de Utilização:
>>> pecasNorte 0 [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe] 
[Peca Recta 0,Peca (Rampa Norte) 0,Peca (Rampa Sul) 0,Peca (Curva Este) 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1)]

-}
pecasNorte:: Altura -> Caminho -> [Peca]
pecasNorte _ [] = [] 
pecasNorte a (h:t) | h == Avanca   = Peca Recta         (highCheck a Avanca)    : pecasNorte (highCheck a Avanca) t
                   | h == CurvaDir = Peca (Curva Norte) (highCheck a CurvaDir)  : pecasEste  (highCheck a CurvaDir) t
                   | h == CurvaEsq = Peca (Curva Este)  (highCheck a CurvaEsq)  : pecasOeste (highCheck a CurvaEsq) t
                   | h == Sobe     = Peca (Rampa Norte) (highCheck a Sobe)      : pecasNorte (highCheck (afterRise a Sobe) Sobe) t
                   | h == Desce    = Peca (Rampa Sul)   (highCheck a Desce)     : pecasNorte (highCheck a Desce) t 

{- | Recebe uma Altura inicial e um Caminho, devolvendo uma lista de Pecas necessárias para a sua realização. O primeiro Passo é feito para Sul.    

== Exemplo de Utilização:
>>> pecasSul 0 [Avanca,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe] 
[Peca Recta 0,Peca (Rampa Sul) 0,Peca (Rampa Norte) 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca (Curva Oeste) 0,Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1)]

-}
pecasSul :: Altura -> Caminho -> [Peca]
pecasSul _ [] = [] 
pecasSul a (h:t)   | h == Avanca   = Peca Recta         (highCheck a Avanca)    : pecasSul (highCheck a Avanca) t
                   | h == CurvaDir = Peca (Curva Sul)   (highCheck a CurvaDir)  : pecasOeste (highCheck a CurvaDir) t
                   | h == CurvaEsq = Peca (Curva Oeste) (highCheck a CurvaEsq)  : pecasEste (highCheck a CurvaEsq) t
                   | h == Sobe     = Peca (Rampa Sul)   (highCheck a Sobe)      : pecasSul (highCheck (afterRise a Sobe) Sobe) t
                   | h == Desce    = Peca (Rampa Norte) (highCheck a Desce)     : pecasSul (highCheck a Desce) t

-- *** Substituição Sucessiva de Peças
  
{- | Recursivamente, faz a substituição sucessiva de Pecas em determinadas posições de um Tabuleiro

== Exemplo de Utilização:
>>> editTabuleiro [[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0]][(Peca Recta 1),(Peca Recta 1)] [(1,1),(1,2)]
[[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0]]

-}
editTabuleiro :: Tabuleiro -> [Peca] -> [Posicao] -> Tabuleiro
editTabuleiro tab [] [] = tab
editTabuleiro tab p ((x,y):t) = editTabuleiro (insertPeca tab (head p) (x,y)) (tail p) t

-- **** Substituição Local de uma Peça
{- | Faz a alteração de uma Peca numa Posicao especifica do Tabuleiro. 

== Exemplo de Utilização
>>> insertPeca  [[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0]] (Peca Recta 1) (1,1)
[[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-}  
insertPeca :: Tabuleiro -> Peca -> Posicao -> Tabuleiro
insertPeca tab p (x,y) = take y tab ++ [take x (tab !! y) ++ [p] ++ drop (x + 1) (tab !! y)] ++ drop (y + 1) tab

-- *** Mudança de Altura

{- | Expressa a mudança de Altura provacada por um Passo.

== Exemplo de Utilização:
>>> highCheck 0 Sobe
0

>>> highCheck 0  Desce
-1

-}
highCheck:: Altura -> Passo -> Altura
highCheck x p  | p == Desce = x - 1  
               | otherwise = x  
           
-- **** Altura depois do Passo Sobe

{- | Certifica que a Altura sobe um valor na Peca seguinte a um Passo do tipo Sobe.

== Exemplo de Utilização: 
>>> afterRise 0 Sobe
1

-}
afterRise :: Altura -> Passo -> Altura
afterRise x Sobe = x + 1



