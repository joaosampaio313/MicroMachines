{-|
Module      : Tarefa5_2017li1g13
Description : Módulo da Tarefa 5 para LI1 17/18

Módulo para a realização da Tarefa 5 de LI1 em 2017/18.
-}
module Main where

import LI11718
import Graphics.Gloss
import Graphics.Gloss.Data.Picture          -- importar o tipo Picture
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import Data.Maybe
import Data.Function
import Data.List.Split
import Tarefa1_2017li1g13 
import Tarefa2_2017li1g13 
import Tarefa3_2017li1g13
import Tarefa4_2017li1g13
import Tarefa6_2017li1g13

-- | Estado do jogo:
data Estado = Estado
              { jogo     :: Jogo
              , actions  :: Actions
              , images   :: Images
              , nPlayer  :: Int        -- ^ Indica o número de jogadores não bot presentes
              , camera   :: Camera
              , winSize  :: (Int, Int) -- ^ Contém o tamanho da janela em que o jogo está a correr
              , pausa    :: Bool       -- ^ Indica se está em pausa ou não
              , inCountDown :: Bool    -- ^ Indica se está em contagem decrescente para começar ou não
              , tToStart    :: Float   -- ^ Indica quanto tempo falta para começar
              , tFromStart  :: Float   -- ^ Indica quanto tempo é que passou desde que o jogo começou
              , inMenu      :: Bool    -- ^ Indica se está dentro do Menu ou não
              , whereMenu   :: MenuPos -- ^ Indica a posição dentro do Menu
              , pMapa       :: Caminho -- ^ Indica o caminho que é construído pelo utilizador
              , nVoltas     :: Int     -- ^ Indica o número de VOltas a dar pelo utilizador
              , idPath      :: [Posicao]  -- ^ Indica o caminho ideal de uma volta à pista (utilizado para contar voltas) 
              }

-- | Record que contém as 'Acao' de todos os jogadores.
data Actions = Actions
               { a1 :: Acao
               , a2 :: Acao
               , a3 :: Acao
               , a4 :: Acao
               }

-- | Record que contém todas as 'Picture' a utilizar.
data Images = Images
                { p1              :: Picture
                , p2              :: Picture
                , p3              :: Picture
                , p4              :: Picture
                , p1N             :: Picture
                , p2N             :: Picture
                , p3N             :: Picture
                , p4N             :: Picture
                , id1             :: Picture
                , id2             :: Picture
                , id3             :: Picture
                , id4             :: Picture
                , back            :: Picture
                , helpMenu        :: Picture
                , pauseMenu       :: Picture
                , inicialStart    :: Picture
                , inicialHelp     :: Picture
                , propAsfalto     :: Picture
                , propTerra       :: Picture
                , propGelo        :: Picture
                , propBack        :: Picture
                , dificultyEasy   :: Picture
                , dificultyMedium :: Picture
                , dificultyHard   :: Picture
                , dificultyBack   :: Picture
                , nPlayer1        :: Picture
                , nPlayer2        :: Picture
                , nPlayerBack     :: Picture
                , t3              :: Picture
                , t2              :: Picture
                , t1              :: Picture
                , tGo             :: Picture
                , spMInval        :: Picture
                , extraDif        :: Picture
                , extraCreate     :: Picture
                , lap1            :: Picture
                , lap2            :: Picture
                , finalLap        :: Picture
                }

-- | Variável para os angulo de camera disponíveis.
data Camera = Full     -- ^ O 'Mapa' todo 
            | Close    -- ^ Zoom sobre o jogador 
            | BirdEye  -- ^ Seguir o jogador visto de cima 
            deriving Eq

-- | Variável para a posição do menu em que o jogo está.
data MenuPos = MenuInicialStart 
             | MenuInicialHelp 
             | MenuHelp 
             | MenuPropAsfalto 
             | MenuPropTerra 
             | MenuPropGelo 
             | MenuPropReturn 
             | MenuDificultyEasy 
             | MenuDificultyMedium 
             | MenuDificultyHard 
             | MenuDificultyReturn 
             | MenuNPLayer1 
             | MenuNPLayer2 
             | MenuNPLayerReturn
             | SpecialMap
             | SpecialMapInval
             | EndGame
             deriving Eq
{-|
Função principal usada para animar um jogo completo.
Compilar com o GHC.
-}
main :: IO ()
main = do inicio <- estadoInicial
          joga inicio

-- | Função que controla o jogo
joga:: Estado -> IO()
joga inicio = play
      (InWindow  "Micro Machines" (1280,720) (0,0) )                -- Janela onde irá correr o jogo      
      (greyN 0.8)                                                   -- Cor do fundo da janela.
      60                                                            -- Frame Rate
      inicio                                                        -- Fundo inicial do jogo.
      desenhaEstado                                                 -- Desenha o Estado do jogo.
      reageEvento                                                   -- Reage a um Evento.
      reageTempo                                                    -- Reage ao passar do Tempo.

-- | Função que devolve uma Picture a partir de um Estado.
estadoInicial :: IO Estado
estadoInicial = do  vermelho    <- loadBMP "sprite/carros/car/vermelho.bmp"
                    azul        <- loadBMP "sprite/carros/car/azul.bmp"
                    verde       <- loadBMP "sprite/carros/car/verde.bmp"
                    branco      <- loadBMP "sprite/carros/car/branco.bmp"
                    vermelhoNit <- loadBMP "sprite/carros/carNit/vermelhoNitro.bmp"
                    azulNit     <- loadBMP "sprite/carros/carNit/azulNitro.bmp"
                    verdeNit    <- loadBMP "sprite/carros/carNit/verdeNitro.bmp"
                    brancoNit   <- loadBMP "sprite/carros/carNit/brancoNitro.bmp"
                    p1Id        <- loadBMP "sprite/carros/indicators/indicatorP1.bmp"
                    p2Id        <- loadBMP "sprite/carros/indicators/indicatorP2.bmp"
                    p3Id        <- loadBMP "sprite/carros/indicators/indicatorP3.bmp"
                    p4Id        <- loadBMP "sprite/carros/indicators/indicatorP4.bmp"
                    lava        <- loadBMP "sprite/background/lava.bmp"
                    help        <- loadBMP "sprite/menu/helpMenu.bmp"
                    pausa       <- loadBMP "sprite/menu/pausedMenu.bmp"
                    countDown1  <- loadBMP "sprite/menu/countDown/countDown1.bmp"
                    countDown2  <- loadBMP "sprite/menu/countDown/countDown2.bmp"
                    countDown3  <- loadBMP "sprite/menu/countDown/countDown3.bmp"
                    countDownGo <- loadBMP "sprite/menu/countDown/countDownGo.bmp" 
                    menuInicialStart    <- loadBMP "sprite/menu/initialMenu/initialMenuStart.bmp"
                    menuInicialHelp     <- loadBMP "sprite/menu/initialMenu/initialMenuHelp.bmp"
                    menuPropAsfalto     <- loadBMP "sprite/menu/propMenu/propMenuAsfalto.bmp"
                    menuPropTerra       <- loadBMP "sprite/menu/propMenu/propMenuTerra.bmp"
                    menuPropGelo        <- loadBMP "sprite/menu/propMenu/propMenuGelo.bmp"
                    menuPropBack        <- loadBMP "sprite/menu/propMenu/propMenuBack.bmp"
                    menuDificultyEasy   <- loadBMP "sprite/menu/dificultyMenu/dificultyMenuEasy.bmp" 
                    menuDificultyMedium <- loadBMP "sprite/menu/dificultyMenu/dificultyMenuMedium.bmp"
                    menuDificultyHard   <- loadBMP "sprite/menu/dificultyMenu/dificultyMenuHard.bmp"
                    menuDificultyBack   <- loadBMP "sprite/menu/dificultyMenu/dificultyMenuBack.bmp"
                    menuNPLayer1        <- loadBMP "sprite/menu/nPlayerMenu/nPlayerMenu1.bmp"
                    menuNPLayer2        <- loadBMP "sprite/menu/nPlayerMenu/nPlayerMenu2.bmp"
                    menuNPLayerBack     <- loadBMP "sprite/menu/nPlayerMenu/nPlayerMenuBack.bmp"
                    menuInval           <- loadBMP "sprite/menu/menuInval.bmp"
                    menuExtraDif        <- loadBMP "sprite/menu/dificultyMenu/addonDificulty.bmp"
                    menuExtraCreate     <- loadBMP "sprite/menu/addonCreate.bmp"
                    menuLap1            <- loadBMP "sprite/menu/lapCount/lap1.bmp"
                    menuLap2            <- loadBMP "sprite/menu/lapCount/lap2.bmp"
                    menuLap3            <- loadBMP "sprite/menu/lapCount/finallap.bmp"
                    let defaultCar    = Carro {posicao = (0,0), direcao = 0, velocidade = (0.0,0.0)}
                    let defaultCarros = [defaultCar, defaultCar , defaultCar , defaultCar]
                    let defaultHist   = [[(0,0)],[(0,0)],[(0,0)],[(0,0)]]
                    let imageList     = Images branco verde azul vermelho brancoNit verdeNit azulNit vermelhoNit p1Id p2Id p3Id p4Id  lava help pausa menuInicialStart menuInicialHelp menuPropAsfalto menuPropTerra menuPropGelo menuPropBack menuDificultyEasy menuDificultyMedium menuDificultyHard menuDificultyBack menuNPLayer1 menuNPLayer2 menuNPLayerBack countDown3 countDown2 countDown1 countDownGo menuInval menuExtraDif menuExtraCreate menuLap1 menuLap2 menuLap3
                    return Estado {jogo    = Jogo mapaEmpty  asfalto defaultCarros tNitI defaultHist,
                                   actions = defaultActions,
                                   images  = imageList,
                                   nPlayer = 1,
                                   camera  = Full,
                                   winSize = (0,0),
                                   pausa   = False,
                                   inCountDown = True,
                                   tToStart    = 4,
                                   tFromStart  = 0,
                                   inMenu      = True,
                                   whereMenu   = MenuInicialStart,
                                   pMapa       = [],
                                   nVoltas     = 0,
                                   idPath      = []}

-- | Lista que contem o tempo de nitros default
tNitI:: [Tempo]
tNitI = [5.0,5.0,5.0,5.0]

-- | 'Actions' satandard para o início do jogo
defaultActions:: Actions
defaultActions = Actions { a1 = defaultAct, a2 = defaultAct, a3 = defaultAct, a4 = defaultAct}

-- | 'Acao' satandard para o início do jogo
defaultAct:: Acao
defaultAct = Acao {acelerar = False, travar = False, esquerda = False, direita = False, nitro = Nothing}

-- * Reagir a um 'Event'
-- | Função que altera o 'Estado' quando acontece um evento.                     
reageEvento :: Event -> Estado -> Estado
reageEvento (EventResize size) e = e {winSize = size}
reageEvento (EventKey (SpecialKey KeyTab)    Down _ _) e = e {inMenu = True, whereMenu = MenuInicialStart} 
reageEvento ev e | inMenu e      = reageEventoMenu ev e
                 | inCountDown e = e
                 | otherwise     = reageEventoJogo ev e

-- ** Reagir a um 'Event' no Menu
-- | Função que, assumindo que o jogador está no Menu, altera o estado do jogo quando ocorre um evento
reageEventoMenu:: Event -> Estado -> Estado
reageEventoMenu (EventKey (SpecialKey KeyUp)  Down _ _) e | whereMenu e == MenuInicialStart    = e { whereMenu = MenuInicialHelp} 
                                                          | whereMenu e == MenuInicialHelp     = e { whereMenu = MenuInicialStart}
                                                          | whereMenu e == MenuPropAsfalto     = e { whereMenu = MenuPropReturn}
                                                          | whereMenu e == MenuPropTerra       = e { whereMenu = MenuPropAsfalto}
                                                          | whereMenu e == MenuPropGelo        = e { whereMenu = MenuPropTerra}
                                                          | whereMenu e == MenuPropReturn      = e { whereMenu = MenuPropGelo}
                                                          | whereMenu e == MenuDificultyEasy   = e { whereMenu = MenuDificultyReturn}
                                                          | whereMenu e == MenuDificultyMedium = e { whereMenu = MenuDificultyEasy }
                                                          | whereMenu e == MenuDificultyHard   = e { whereMenu = MenuDificultyMedium}
                                                          | whereMenu e == MenuDificultyReturn = e { whereMenu = MenuDificultyHard}
                                                          | whereMenu e == MenuNPLayer1        = e { whereMenu = MenuNPLayerReturn}
                                                          | whereMenu e == MenuNPLayer2        = e { whereMenu = MenuNPLayer1}
                                                          | whereMenu e == MenuNPLayerReturn   = e { whereMenu = MenuNPLayer2}
                                                          | whereMenu e == SpecialMap          = e { pMapa = pMapa e ++ [Avanca]}
                                                          | whereMenu e == SpecialMapInval     = e {whereMenu = SpecialMap} 

reageEventoMenu (EventKey (SpecialKey KeyDown) Down _ _) e | whereMenu e == MenuInicialStart    = e { whereMenu = MenuInicialHelp}
                                                           | whereMenu e == MenuInicialHelp     = e { whereMenu = MenuInicialStart}
                                                           | whereMenu e == MenuPropAsfalto     = e { whereMenu = MenuPropTerra}
                                                           | whereMenu e == MenuPropTerra       = e { whereMenu = MenuPropGelo}
                                                           | whereMenu e == MenuPropGelo        = e { whereMenu = MenuPropReturn}
                                                           | whereMenu e == MenuPropReturn      = e { whereMenu = MenuPropAsfalto}
                                                           | whereMenu e == MenuDificultyEasy   = e { whereMenu = MenuDificultyMedium}
                                                           | whereMenu e == MenuDificultyMedium = e { whereMenu = MenuDificultyHard}
                                                           | whereMenu e == MenuDificultyHard   = e { whereMenu = MenuDificultyReturn}
                                                           | whereMenu e == MenuDificultyReturn = e { whereMenu = MenuDificultyEasy}
                                                           | whereMenu e == MenuNPLayer1        = e { whereMenu = MenuNPLayer2}
                                                           | whereMenu e == MenuNPLayer2        = e { whereMenu = MenuNPLayerReturn}
                                                           | whereMenu e == MenuNPLayerReturn   = e { whereMenu = MenuNPLayer1}
                                                           | whereMenu e == SpecialMapInval     = e {whereMenu = SpecialMap} 

reageEventoMenu (EventKey (SpecialKey KeyLeft) Down _ _) e | whereMenu e == SpecialMap          = e { pMapa = pMapa e ++ [CurvaEsq]}
                                                           | whereMenu e == SpecialMapInval     = e {whereMenu = SpecialMap} 

reageEventoMenu (EventKey (SpecialKey KeyRight) Down _ _) e | whereMenu e == SpecialMap          = e { pMapa = pMapa e ++ [CurvaDir]}
                                                            | whereMenu e == SpecialMapInval     = e {whereMenu = SpecialMap} 

reageEventoMenu (EventKey (Char 'w')    Down _ _) e       | whereMenu e == MenuInicialStart    = e { whereMenu = MenuInicialHelp} 
                                                          | whereMenu e == MenuInicialHelp     = e { whereMenu = MenuInicialStart}
                                                          | whereMenu e == MenuPropAsfalto     = e { whereMenu = MenuPropReturn}
                                                          | whereMenu e == MenuPropTerra       = e { whereMenu = MenuPropAsfalto}
                                                          | whereMenu e == MenuPropGelo        = e { whereMenu = MenuPropTerra}
                                                          | whereMenu e == MenuPropReturn      = e { whereMenu = MenuPropGelo}
                                                          | whereMenu e == MenuDificultyEasy   = e { whereMenu = MenuDificultyReturn}
                                                          | whereMenu e == MenuDificultyMedium = e { whereMenu = MenuDificultyEasy }
                                                          | whereMenu e == MenuDificultyHard   = e { whereMenu = MenuDificultyMedium}
                                                          | whereMenu e == MenuDificultyReturn = e { whereMenu = MenuDificultyHard}
                                                          | whereMenu e == MenuNPLayer1        = e { whereMenu = MenuNPLayerReturn}
                                                          | whereMenu e == MenuNPLayer2        = e { whereMenu = MenuNPLayer1}
                                                          | whereMenu e == MenuNPLayerReturn   = e { whereMenu = MenuNPLayer2}
                                                          | whereMenu e == SpecialMap          = e { pMapa = pMapa e ++ [Sobe]}
                                                          | whereMenu e == SpecialMapInval     = e {whereMenu = SpecialMap} 

reageEventoMenu (EventKey (Char 's')  Down _ _) e         | whereMenu e == MenuInicialStart    = e { whereMenu = MenuInicialHelp}
                                                          | whereMenu e == MenuInicialHelp     = e { whereMenu = MenuInicialStart}
                                                          | whereMenu e == MenuPropAsfalto     = e { whereMenu = MenuPropTerra}
                                                          | whereMenu e == MenuPropTerra       = e { whereMenu = MenuPropGelo}
                                                          | whereMenu e == MenuPropGelo        = e { whereMenu = MenuPropReturn}
                                                          | whereMenu e == MenuPropReturn      = e { whereMenu = MenuPropAsfalto}
                                                          | whereMenu e == MenuDificultyEasy   = e { whereMenu = MenuDificultyMedium}
                                                          | whereMenu e == MenuDificultyMedium = e { whereMenu = MenuDificultyHard}
                                                          | whereMenu e == MenuDificultyHard   = e { whereMenu = MenuDificultyReturn}
                                                          | whereMenu e == MenuDificultyReturn = e { whereMenu = MenuDificultyEasy}
                                                          | whereMenu e == MenuNPLayer1        = e { whereMenu = MenuNPLayer2}
                                                          | whereMenu e == MenuNPLayer2        = e { whereMenu = MenuNPLayerReturn}
                                                          | whereMenu e == MenuNPLayerReturn   = e { whereMenu = MenuNPLayer1}
                                                          | whereMenu e == SpecialMap          = e { pMapa = pMapa e ++ [Desce]}
                                                          | whereMenu e == SpecialMapInval     = e {whereMenu = SpecialMap} 

reageEventoMenu (EventKey (Char 'b')          Down _ _) e | whereMenu e == MenuInicialStart    = e { whereMenu = MenuInicialStart}
                                                          | whereMenu e == MenuInicialHelp     = e { whereMenu = MenuInicialHelp}
                                                          | whereMenu e == MenuHelp            = e { whereMenu = MenuInicialHelp}
                                                          | whereMenu e == MenuPropAsfalto     = e { whereMenu = MenuInicialStart}
                                                          | whereMenu e == MenuPropTerra       = e { whereMenu = MenuInicialStart}
                                                          | whereMenu e == MenuPropGelo        = e { whereMenu = MenuInicialStart}
                                                          | whereMenu e == MenuPropReturn      = e { whereMenu = MenuInicialStart}
                                                          | whereMenu e == MenuDificultyEasy   = e { whereMenu = MenuPropAsfalto}
                                                          | whereMenu e == MenuDificultyMedium = e { whereMenu = MenuPropAsfalto}
                                                          | whereMenu e == MenuDificultyHard   = e { whereMenu = MenuPropAsfalto}
                                                          | whereMenu e == MenuDificultyReturn = e { whereMenu = MenuPropAsfalto}
                                                          | whereMenu e == MenuNPLayer1        = e { whereMenu = MenuDificultyEasy}
                                                          | whereMenu e == MenuNPLayer2        = e { whereMenu = MenuDificultyEasy}
                                                          | whereMenu e == MenuNPLayerReturn   = e { whereMenu = MenuDificultyEasy}
                                                          | whereMenu e == SpecialMap          = e { whereMenu = MenuPropAsfalto}

reageEventoMenu (EventKey (SpecialKey KeyEnter) Down _ _) e| whereMenu e == MenuInicialStart    = e { whereMenu = MenuPropAsfalto}
                                                           | whereMenu e == MenuInicialHelp     = e { whereMenu = MenuHelp}
                                                           | whereMenu e == MenuHelp            = e { whereMenu = MenuInicialHelp}
                                                           | whereMenu e == MenuPropAsfalto     = e { whereMenu = MenuDificultyEasy, jogo = (jogo e) {pista = asfalto}}
                                                           | whereMenu e == MenuPropTerra       = e { whereMenu = MenuDificultyEasy, jogo = (jogo e) {pista = terra}}
                                                           | whereMenu e == MenuPropGelo        = e { whereMenu = MenuDificultyEasy, jogo = (jogo e) {pista = gelo}}
                                                           | whereMenu e == MenuPropReturn      = e { whereMenu = MenuInicialStart}
                                                           | whereMenu e == MenuDificultyEasy   = e { whereMenu = MenuNPLayer1, jogo = jogoNovo, nVoltas = 3}
                                                           | whereMenu e == MenuDificultyMedium = e { whereMenu = MenuNPLayer1, jogo = jogoNovo, nVoltas = 2}
                                                           | whereMenu e == MenuDificultyHard   = e { whereMenu = MenuNPLayer1, jogo = jogoNovo, nVoltas = 1}
                                                           | whereMenu e == MenuDificultyReturn = e { whereMenu = MenuPropAsfalto}
                                                           | whereMenu e == MenuNPLayer1        = e { inMenu    = False, nPlayer = 1, camera = Full, inCountDown = True, tToStart = 4, tFromStart = 0, actions = defaultActions, idPath = map fromJust idPathNovo}
                                                           | whereMenu e == MenuNPLayer2        = e { inMenu    = False, nPlayer = 2, camera = Full, inCountDown = True, tToStart = 4, tFromStart = 0, actions = defaultActions, idPath = map fromJust idPathNovo}
                                                           | whereMenu e == MenuNPLayerReturn   = e { whereMenu = MenuDificultyEasy}
                                                           | whereMenu e == SpecialMap          = (specialMapCheck e){nVoltas = 2}
                                                           | whereMenu e == SpecialMapInval     = e {whereMenu = SpecialMap}
                                                           | whereMenu e == EndGame             = e {whereMenu = MenuInicialStart}
            where
              mapaSelecionado = escolheMapa (whereMenu e) (pista (jogo e))
              jogoNovo        = gameCreator (jogo e) mapaSelecionado
              idPathNovo      = reverse (idPathGenerator (mapa (jogo e)))

reageEventoMenu (EventKey (Char 'c')    Down _ _) e | whereMenu e == SpecialMap      = e{pMapa = []}
                                                    | whereMenu e == SpecialMapInval = e{pMapa = [], whereMenu = SpecialMap} 

reageEventoMenu (EventKey (Char 'z')    Down _ _) e | whereMenu e == SpecialMap = e {pMapa = init (pMapa e) } 

reageEventoMenu (EventKey (Char 'p') Up   _ _) e | whereMenu e == MenuDificultyEasy   = e { whereMenu = SpecialMap, pMapa = []}
                                                 | whereMenu e == MenuDificultyMedium = e { whereMenu = SpecialMap, pMapa = []}
                                                 | whereMenu e == MenuDificultyHard   = e { whereMenu = SpecialMap, pMapa = []}
reageEventoMenu _ e  = e            

-- | Função que verifica se o mapa gerado pelo utilizador é válido.
specialMapCheck:: Estado -> Estado
specialMapCheck e | valida (constroi (pMapa e)) = e {jogo = gameCreator (jogo e) (constroi (pMapa e)), whereMenu = MenuNPLayer1 }
                  | otherwise                   = e {whereMenu = SpecialMapInval}

-- | Função que gera o jogo, ou seja, faz reset da quantidade de nitro e da posição , velocidade e orientação dos carros.
gameCreator:: Jogo -> Mapa -> Jogo
gameCreator j m = j  {mapa = m, carros = defaultCarros,nitros = tNitI, historico = defaultHist }
            where
              pPartida      = mapPosInicial m
              (x,y)         = pPartida
              o             = mapOrInicial m
              a             = orToAng o
              p1            = (0.1 + fromIntegral x, 0.2 + fromIntegral y)
              p2            = (0.1 + fromIntegral x, 0.4 + fromIntegral y)
              p3            = (0.1 + fromIntegral x, 0.6 + fromIntegral y)
              p4            = (0.1 + fromIntegral x, 0.8 + fromIntegral y)
              defaultCar1   = Carro {posicao = p1, direcao = a, velocidade = (0.0,0.0)}
              defaultCar2   = Carro {posicao = p2, direcao = a, velocidade = (0.0,0.0)}
              defaultCar3   = Carro {posicao = p3, direcao = a, velocidade = (0.0,0.0)}
              defaultCar4   = Carro {posicao = p4, direcao = a, velocidade = (0.0,0.0)}
              defaultCarros = [defaultCar1, defaultCar2, defaultCar3, defaultCar4]
              defaultHist   = [[pPartida],[pPartida],[pPartida],[pPartida]]

-- | Função que gra o caminho que os carros têm de percorrer paa contar uma volta.
idPathGenerator:: Mapa -> [Maybe Posicao]
idPathGenerator m@(Mapa (p,o) t) = tail almostFinal ++ [head almostFinal] ++ [Just p]
            where
                nAvancos         = 2 * (nBlocosCaminho m + 1)
                coorAntesPartida = nudge p (orOposta o)
                rawListCoor      = percorreMapa m  (coorAntesPartida, o) nAvancos 1
                almostFinal      = unique rawListCoor
-- *** Escolher um Mapa
-- | Função que, assumindo que o jogador está a correr, altera o estado do jogo quando ocorre um evento
escolheMapa:: MenuPos -> Propriedades -> Mapa
escolheMapa MenuDificultyEasy   p | p == asfalto = asfaltoEasy
escolheMapa MenuDificultyEasy   p | p == terra   = terraEasy
escolheMapa MenuDificultyEasy   p | p == gelo    = geloEasy
escolheMapa MenuDificultyMedium p | p == asfalto = asfaltoMedium
escolheMapa MenuDificultyMedium p | p == terra   = terraMedium
escolheMapa MenuDificultyMedium p | p == gelo    = geloMedium
escolheMapa MenuDificultyHard   p | p == asfalto = asfaltoHard
escolheMapa MenuDificultyHard   p | p == terra   = terraHard
escolheMapa MenuDificultyHard   p | p == gelo    = geloHard
escolheMapa _                   _                = asfaltoEasy

-- ** Reagir a um 'Event' em jogo
-- | Função que, assumindo que o jogo está em corrida, altera o estado do jogo quando ocorre um evento
reageEventoJogo:: Event -> Estado -> Estado
reageEventoJogo (EventKey (SpecialKey KeyUp)    Down _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {acelerar = True }}} 
reageEventoJogo (EventKey (SpecialKey KeyUp)    Up   _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {acelerar = False}}} 
reageEventoJogo (EventKey (SpecialKey KeyDown)  Down _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {travar   = True }}} 
reageEventoJogo (EventKey (SpecialKey KeyDown)  Up   _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {travar   = False}}} 
reageEventoJogo (EventKey (SpecialKey KeyLeft)  Down _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {esquerda = True }}} 
reageEventoJogo (EventKey (SpecialKey KeyLeft)  Up   _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {esquerda = False}}} 
reageEventoJogo (EventKey (SpecialKey KeyRight) Down _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {direita  = True }}} 
reageEventoJogo (EventKey (SpecialKey KeyRight) Up   _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {direita  = False}}}
reageEventoJogo (EventKey (Char       'm')      Down _ _) e |head (nitros (jogo e)) == 0 = e
                                                            |otherwise = e {actions = (actions e) {a1 = (a1 (actions e)) {nitro    = Just 0}}}
reageEventoJogo (EventKey (Char       'm')      Up   _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {nitro    = Nothing}}}

reageEventoJogo (EventKey (Char       ',')      Down _ _) e |head (nitros (jogo e)) == 0 = e
                                                            |otherwise = e {actions = (actions e) {a1 = (a1 (actions e)) {nitro    = Just 1}}}
reageEventoJogo (EventKey (Char       ',')      Up   _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {nitro    = Nothing}}} 

reageEventoJogo (EventKey (Char       '.')      Down _ _) e |head (nitros (jogo e)) == 0 = e
                                                            |otherwise = e {actions = (actions e) {a1 = (a1 (actions e)) {nitro    = Just 2}}}

reageEventoJogo (EventKey (Char       '.')      Up   _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {nitro    = Nothing}}}

reageEventoJogo (EventKey (Char       '-')      Down _ _) e |head (nitros (jogo e)) == 0 = e
                                                            |otherwise = e {actions = (actions e) {a1 = (a1 (actions e)) {nitro    = Just 3}}}
reageEventoJogo (EventKey (Char       '-')      Up   _ _) e = e {actions = (actions e) {a1 = (a1 (actions e)) {nitro    = Nothing}}} 

reageEventoJogo (EventKey (Char 'w') Down _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {acelerar = True }}} 
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 'w') Up   _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {acelerar = False}}} 
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 's') Down _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {travar   = True }}} 
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 's') Up   _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {travar   = False}}} 
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 'a') Down _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {esquerda = True }}} 
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 'a') Up   _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {esquerda = False}}} 
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 'd') Down _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {direita  = True }}} 
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 'd') Up   _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {direita  = False}}} 
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 'e') Down _ _) e | nitros (jogo e)!!1 == 0 = e
                                                 | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {nitro    = Just 0}}}
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 'e') Up   _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {nitro    = Nothing}}}
                                                 | otherwise      = e

reageEventoJogo (EventKey (Char 'r') Down _ _) e | nitros (jogo e)!!1 == 0 = e
                                                 | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {nitro    = Just 1}}}
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 'r') Up   _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {nitro    = Nothing}}}
                                                 | otherwise      = e

reageEventoJogo (EventKey (Char 't') Down _ _) e | nitros (jogo e)!!1 == 0 = e
                                                 | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {nitro    = Just 2}}}
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 't') Up   _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {nitro    = Nothing}}}
                                                 | otherwise      = e                                                 

reageEventoJogo (EventKey (Char 'y') Down _ _) e | nitros (jogo e)!!1 == 0 = e
                                                 | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {nitro    = Just 3}}}
                                                 | otherwise      = e
reageEventoJogo (EventKey (Char 'y') Up   _ _) e | nPlayer e == 2 = e {actions = (actions e) {a2 = (a2 (actions e)) {nitro    = Nothing}}}
                                                 | otherwise      = e

reageEventoJogo (EventKey (Char 'v') Up   _ _) e | camera e == Full    = e{camera = Close} 
                                                 | camera e == Close   = e{camera = BirdEye}
                                                 | camera e == BirdEye = e{camera = Full}
reageEventoJogo (EventKey (SpecialKey KeyTab)    Down _ _) e = e {inMenu = True, whereMenu = MenuInicialStart} 
reageEventoJogo (EventKey (Char 'p') Up   _ _) e = e {pausa = not (pausa e)}
reageEventoJogo _ e = e

-- * Reagir a Tempo
-- | Função que altera o 'Estado' do jogo com base no tempo que passou 
reageTempo :: Float -> Estado -> Estado
reageTempo tick e | inMenu e      = e
                  | pausa  e      = e
                  | inCountDown e = reageTempoCountDown tick e
                  | otherwise     = reageTempoGame      tick e

-- ** Alterar valor contagem decrescente
{- | Função que recebe o tempo que passou e um 'Estado' e devolve um 'Estado' novo em que o tempo de contagem decrescente para a partida está atualizado.
Note que quando o tempo passa de zero, o valor no 'Estado' fica zero e o valor inCountDown fica False
-}
reageTempoCountDown:: Float -> Estado -> Estado
reageTempoCountDown tick e | ab dt == 0 = e {inCountDown = False, tToStart = 0}
                           | otherwise  = e {tToStart = dt}
            where
                t  = tToStart e
                dt = t - tick
                ab:: Float -> Float
                ab x | x < 0 = 0
                     | otherwise = x 

-- ** Reage ao Tempo quando está dentro do jogo
{- | Funcão que recebe um 'Estado' e o tempo que passa e devolve o 'Estado' ao fim desse tempo
-}
reageTempoGame:: Float -> Estado -> Estado
reageTempoGame tick e = checkLaps addTime
            where
                botAplied       = reageTempoBots tick e
                actSemMovimenta = reageTempoAtualiza tick botAplied
                actualizado     = reageTempoMovimenta tick actSemMovimenta
                rNit            = restoreNits actualizado
                addTime         = rNit {tFromStart = tFromStart actualizado + tick}               

-- *** Recuperação de Nitro
-- | Função que restaura o nitro de todos os jogadores quando cada um passa pela partida.
restoreNits:: Estado -> Estado
restoreNits e = r4
            where
                r1 = restoreNit e  0
                r2 = restoreNit r1 1
                r3 = restoreNit r2 2
                r4 = restoreNit r3 3

-- | Função que coloca nitro num carro específico quando este passa pela partida.
restoreNit::Estado -> Int -> Estado 
restoreNit e i | currPos == p && length carHist > 1 = e {jogo = (jogo e){nitros = atList (nitros (jogo e)) 5 i}}
               | otherwise                            = e 
            where
                carHist = historico (jogo e) !! i
                currPos = head carHist
                (Mapa (p,o) t)= mapa (jogo e)

-- *** controlar carros
-- **** Implementação da 'Tarefa6_2017li1g13'
-- | Função que recebe um estado e controla a 'Acao' dos bot
reageTempoBots::Float -> Estado -> Estado
reageTempoBots tick e | nPlayer e == 1 = bot3
                      | nPlayer e == 2 = bot2
            where
                bot1 = e    {actions = (actions e   ){a4 = bot (realToFrac tick) (jogo e   ) 3} }
                bot2 = bot1 {actions = (actions bot1){a3 = bot (realToFrac tick) (jogo bot1) 2} }
                bot3 = bot2 {actions = (actions bot2){a2 = bot (realToFrac tick) (jogo bot2) 1} }

-- **** Implementação da 'Tarefa4_2017li1g13'
-- | Função que recebe um estado e actualiza os carros
reageTempoAtualiza::Float -> Estado -> Estado
reageTempoAtualiza tick e = act4
            where
              act1 = e    {jogo = atualiza (realToFrac tick) (jogo e)    0 (a1 (actions e))}
              act2 = act1 {jogo = atualiza (realToFrac tick) (jogo act1) 1 (a2 (actions e))} 
              act3 = act2 {jogo = atualiza (realToFrac tick) (jogo act2) 2 (a3 (actions e))}
              act4 = act3 {jogo = atualiza (realToFrac tick) (jogo act3) 3 (a4 (actions e))}

-- **** Implementação da 'Tarefa3_2017li1g13'
-- | Função que recebe um estado e movimenta os carros
reageTempoMovimenta:: Float -> Estado -> Estado
reageTempoMovimenta tick e = e{jogo = (jogo e) {carros = [moveCar1, moveCar2, moveCar3, moveCar4]}}
            where
                map  = mapa(jogo e)
                game = jogo e
                listCar = carros game
                moveCar1 = movimentar map tick (head listCar) (head (head (historico game)))  
                moveCar2 = movimentar map tick (listCar !! 1) (head (historico game !! 1))
                moveCar3 = movimentar map tick (listCar !! 2) (head (historico game !! 2))
                moveCar4 = movimentar map tick (listCar !! 3) (head (historico game !! 3))

{- | Função que recebe um 'Mapa', o tempo que passou, um 'Carro' e a 'Posicao' que corresponde à ultima posição onde o carro esteve e devolve o 'Carro' movimentado.
Note que caso o 'Carro' seja destruído, ele é colocado na última peca do histórico com base na função 'colidePosFinal'
-}
movimentar:: Mapa -> Float -> Carro -> Posicao -> Carro
movimentar map tick car (x,y) | isNothing move = car{velocidade = (0,0), posicao = pColide}
                              | otherwise      = fromJust move
            where
              tab     = mapToTab map
              move    =  movimenta tab (realToFrac tick) car
              pColide = colidePosFinal (pecaFinder map (x,y)) (x,y)

-- | Função que devolve o 'Ponto' em que o 'Carro' acaba com base na 'Peca' onde o carro colide e a 'Posicao' desta. 
colidePosFinal:: Peca -> Posicao -> Ponto
colidePosFinal (Peca (Curva Norte) _) (x,y) = (fromIntegral x + 0.7, fromIntegral y + 0.7)  
colidePosFinal (Peca (Curva Sul)   _) (x,y) = (fromIntegral x + 0.3, fromIntegral y + 0.3)
colidePosFinal (Peca (Curva Este)  _) (x,y) = (fromIntegral x + 0.3, fromIntegral y + 0.7)
colidePosFinal (Peca (Curva Oeste) _) (x,y) = (fromIntegral x + 0.7, fromIntegral y + 0.3)
colidePosFinal _                      (x,y) = (fromIntegral x + 0.5, fromIntegral y + 0.5)                   

-- *** Controlo do número de voltas
-- | Função que caso um carro termine o número de voltas previstos coloca o jogo no menu final
checkLaps:: Estado -> Estado
checkLaps e | any (>= n) (lapCountList e) = e {whereMenu = EndGame, inMenu = True}
            | otherwise = e 
          where
              n = nVoltas e

-- | Função que devolve uma Lista com o número de voltas que os carros deram à Pista
lapCountList:: Estado -> [Int]
lapCountList e = map (lapCount e) [0..3]

-- | Função que calcula quantas voltas é que um determinado carro deu à pista
lapCount:: Estado -> Int -> Int
lapCount e i = length (filter (== path) (cutPosLaps carHist p))
            where
                carHist        = historico (jogo e) !! i
                (Mapa (p,o) t) = mapa (jogo e)
                path           = idPath e

-- | Função que corta o historico de 'Posicao' de um 'Carro' em segmentos de possíveis voltas.
cutPosLaps:: [Posicao] -> Posicao -> [[Posicao]]
cutPosLaps l p = map (f p . unique) $ groupBy (\a b -> b /= p) l
            where
                f:: Posicao -> [Posicao] -> [Posicao]
                f e l = l ++ [e]

-- * Desenhar um 'Estado'
-- | Função responsável por desenhar o jogo. Dependendo em que parte do jogo estiver, chama a função 'desenhaMenu' ou a função 'desenhaJogo'  
desenhaEstado :: Estado -> Picture
desenhaEstado e | inMenu e  = desenhaMenu e
                | otherwise = desenhaJogo e

-- ** Desenhar o Menu
-- | Função que recebe um 'Estado', e assumindo que o jogo está na parte dos menus, devolve uma 'Picture' correspondente.
desenhaMenu:: Estado -> Picture 
desenhaMenu e |pos == MenuInicialStart    = scale f f (inicialStart picList) 
              |pos == MenuInicialHelp     = scale f f (inicialHelp  picList)
              |pos == MenuHelp            = scale t t (helpMenu picList)
              |pos == MenuPropAsfalto     = scale f f (propAsfalto picList)
              |pos == MenuPropTerra       = scale f f (propTerra picList) 
              |pos == MenuPropGelo        = scale f f (propGelo picList)
              |pos == MenuPropReturn      = scale f f (propBack picList)
              |pos == MenuDificultyEasy   = scale f f (Pictures [dificultyEasy picList  , extraDif picList]) 
              |pos == MenuDificultyMedium = scale f f (Pictures [dificultyMedium picList, extraDif picList]) 
              |pos == MenuDificultyHard   = scale f f (Pictures [dificultyHard picList  , extraDif picList]) 
              |pos == MenuDificultyReturn = scale f f (Pictures [dificultyBack picList  , extraDif picList])
              |pos == MenuNPLayer1        = scale f f (nPlayer1 picList)
              |pos == MenuNPLayer2        = scale f f (nPlayer2 picList)
              |pos == MenuNPLayerReturn   = scale f f (nPlayerBack picList)
              |pos == SpecialMap          = menuSpecial
              |pos == SpecialMapInval     = Pictures [menuSpecial, scale f f (spMInval picList)]
              |pos == EndGame             = desenhaMenuEndGame e
             where
                 pos     = whereMenu e
                 picList = images e
                 (dx,dy) = winSize e
                 f = minimum[fromIntegral dx / 2048, fromIntegral dy / 2048]
                 t = minimum[fromIntegral dx / 4928, fromIntegral dy / 3264]
                 menuSpecial = Pictures [scale f f (extraCreate picList), drawMapa e {jogo = (jogo e) { mapa = constroi (pMapa e)}}]

-- | Função que desenha o menu final do jogo que indica quem ganhou
desenhaMenuEndGame:: Estado -> Picture
desenhaMenuEndGame e = Pictures [idP4, idP3, idP2, idP1, scale f f enter]
            where
               idP1 = Translate 0  vy        (scale fy fy (Pictures [head listOrder, Translate (-900) (-120) (scale 2 2 (text "1."))]))
               idP2 = Translate 0 (vy -hy)   (scale fy fy (Pictures [listOrder !! 1, Translate (-900) (-120) (scale 2 2 (text "2."))]))
               idP3 = Translate 0 (vy -hy*2) (scale fy fy (Pictures [listOrder !! 2, Translate (-900) (-120) (scale 2 2 (text "3."))]))
               idP4 = Translate 0 (vy -hy*3) (scale fy fy (Pictures [listOrder !! 3, Translate (-900) (-120) (scale 2 2 (text "4."))]))
               (dx,dy) = winSize e
               vx = (fromIntegral dx / 2) - 1298*fy /2 - 20
               vy = (fromIntegral dy / 2) - 450 *fy /2 - 100
               hy = fromIntegral dy / 7
               fy = hy / 450
               f  = minimum[fromIntegral dx / 2048, fromIntegral dy / 2048]
               listOrder = listIds e
               enter = extraCreate (images e)

-- ** Desenhar o Jogo
-- | Função que recebe um 'Estado', e assumindo que o jogo está na parte da pista, devolve uma 'Picture' correspondente.
desenhaJogo:: Estado -> Picture
desenhaJogo e | inCountDown e     = Pictures [picBase, iDs, drawCountDown e]
              | pausa e           = Pictures [picBase, iDs, time,drawLapCount e , scale fPause fPause (pauseMenu (images e))]
              | camera e == Full  = Pictures [picBase, iDs, time,drawLapCount e]
              | camera e == Close   && nPlayer e == 1 = Pictures [closeP1  ,  iDs, time,drawLapCount e]
              | camera e == BirdEye && nPlayer e == 1 = Pictures [birdEyeP1, iDs, time,drawLapCount e]
              | otherwise                             = Pictures [picBase, iDs, time,drawLapCount e]
            where
              iDs     = drawIds e
              picBase = Pictures[drawMapa e, drawCars e]
              anP1    = realToFrac (direcao (head (carros (jogo e)))) - 90
              transP1 = Translate (-1* realToFrac x1) (-1*realToFrac y1) picBase
              closeP1 = scale fzoom fzoom transP1
              birdEyeP1 = Rotate anP1 closeP1
              fzoom     = maximum [fzoomx, fzoomy]
                  where
                      fzoomx = fromIntegral (length (head (mapToTab (mapa (jogo e))))) * fromIntegral pSize / (fromIntegral dx / 4)
                      fzoomy = fromIntegral (length       (mapToTab (mapa (jogo e))))  * fromIntegral pSize / (fromIntegral dy / 4)
              fPause = minimum[fromIntegral dx / 2048, fromIntegral dy / 2048]

              tab   = mapToTab (mapa (jogo e))
              (x1,y1) = cornerToCenter pSize tab $ posicao $ head $ carros $ jogo e
              pSize   = sizePeca (mapToTab (mapa (jogo e))) (winSize e)
              (dx,dy) = winSize e

              time = Translate (10 - fromIntegral dx / 2) (10 - fromIntegral dy / 2) (text (show (rounder (tFromStart e) 2) ++ "s"))

-- *** Desenhar contagem de voltas
-- | Função que desenha a contagem de voltas durante o jogo.
drawLapCount::Estado -> Picture
drawLapCount e | currNLap == nLap = scale f f (finalLap (images e))
               | currNLap == 1    = scale f f (lap1     (images e))
               | currNLap == 2    = scale f f (lap2     (images e))
            where
                f        = minimum[fromIntegral dx / 2048, fromIntegral dy / 2048]
                (dx,dy)  = winSize e
                nLap     = nVoltas e
                currNLap = maximum (lapCountList e) + 1
-- *** Desenhar contagem decrescente

{- | Função que recebe um 'Estado', e assumindo que o jogo está na parte do countDown do jogo, devolve uma 'Picture' correspondente.
Note que a imagem devolvida está escalada à janela onde o jogo está a correr.
-}
drawCountDown:: Estado -> Picture
drawCountDown e | x == 4 = scale f f (t3  (images e))
                | x == 3 = scale f f (t2  (images e))
                | x == 2 = scale f f (t1  (images e))
                | x == 1 = scale f f (tGo (images e)) 
            where
                x = ceiling (tToStart e)
                f = minimum[fromIntegral dx / 2048, fromIntegral dy / 2048]
                (dx,dy) = winSize e

-- *** Desenhar identificadores

{- | Função que recebe um 'Estado', e assumindo que o jogo está na parte dos menus, devolve uma 'Picture' correspondente aos identificadores dos jogadores.
Estes contêm a indicação do nitro restante ao jogador respetivo.
Note que a barra do nitro diminui de forma contínua.
-}
drawIds:: Estado -> Picture
drawIds e = Pictures [idP4, idP3, idP2, idP1]
            where
               idP1 = Translate vx  vy        (scale fy fy (head listOrder))
               idP2 = Translate vx (vy -hy)   (scale fy fy (listOrder !! 1))
               idP3 = Translate vx (vy -hy*2) (scale fy fy (listOrder !! 2))
               idP4 = Translate vx (vy -hy*3) (scale fy fy (listOrder !! 3))
               (dx,dy) = winSize e
               vx      = (fromIntegral dx / 2) - 1298*fy /2 - 20
               vy      = (fromIntegral dy / 2) - 450 *fy /2 - 20
               hy      = fromIntegral dy / 12
               fy      = hy / 450
               listOrder = listIds e

{- | Função que recebe um 'Estado' e devolve a lista de 'Pictures' que corresponde aos dentificadores dos jogadores.
-}
listIds::Estado -> [Picture]
listIds e = organizeIds [(drawId e p1 0,l1), (drawId e p2 1,l2), (drawId e p3 2,l3), (drawId e p4 3,l4)]
            where
                p1 = id1 (images e)
                p2 = id2 (images e)
                p3 = id3 (images e)
                p4 = id4 (images e)
                l1 = length $ head $ historico $ jogo e
                l2 = length (historico (jogo e) !! 1)
                l3 = length (historico (jogo e) !! 2)
                l4 = length (historico (jogo e) !! 3)

-- | Função que rganiza as identificações dos jogadores do que está em priemiro para o que está em último 
organizeIds::[(Picture,Int)] -> [Picture]
organizeIds l = reverse (map fst org)
            where
                org = sortBy (compare `on` snd) l


{- | Função que recebe um 'Estado', uma 'Picture' e o número do jogador e devolve uma 'Picture' que corresponde ao identificador do jogador.
Note que este está centrado no ecrã e não é escalado.
-}
drawId:: Estado -> Picture -> Int -> Picture
drawId e p i = Pictures [p, Translate (-190) (-50) id]
            where
                tNit = nitros (jogo e) !! i
                id   = color c (Polygon [(0,0), (0,-40),(realToFrac tNit * 145, -40),(realToFrac tNit * 150,0) ,(0,0)])
                c    = colorId i

-- *** Desenhar carros
-- | Função que recebe um 'Estado' e devolve uma 'Picture' onde estão representados todos os carros.
drawCars:: Estado -> Picture
drawCars e = Pictures[car4, car3, car2, car1]
            where
                car1    = drawCar e picCars 0 
                car2    = drawCar e picCars 1
                car3    = drawCar e picCars 2
                car4    = drawCar e picCars 3
                picCars = picListCars e

-- | Função que recebe um 'Estado' e devolve uma 'Picture' onde está representado um Carro.
drawCar:: Estado -> [Picture] -> Int -> Picture
drawCar e picCars i = Translate (realToFrac x) (realToFrac y) (Rotate (realToFrac a) pic)
            where
                tab     = mapToTab (mapa (jogo e))
                listCar = carros(jogo e)
                (x,y)   = cornerToCenter pSize tab (posicao (listCar !! i))
                a       = -1 * direcao (listCar !! i)
                pic     = scale fs fs (picCars !! i)
                pSize   = sizePeca (mapToTab (mapa (jogo e))) (winSize e)
                fs      = fromIntegral pSize * 0.50 / 2400

{- | Função que recebe um Estado e devolve uma lista de 'Picture' que corresponde aos carros.
Por exemplo, esta função tem em conta se 'Carro' tem nitro aplicado sobre ele e devolve as imagens.
Note que as 'Picture' estão centradas no ecrã e não têm qualquer scaling, tal é trratado pela função 'drawCar'.
-}
picListCars:: Estado -> [Picture]
picListCars e = [pic1, pic2, pic3, pic4]
            where
              recAct        = actions e
              listAct       = [a1 recAct, a2 recAct, a3 recAct, a4 recAct]
              listNitAct    = map nitro listAct
              listPlayerNit = map fromJust (filter (/= Nothing) listNitAct)
              isP1Nit       = elem 0 listPlayerNit && head (nitros (jogo e)) /= 0 
              isP2Nit       = elem 1 listPlayerNit && (nitros (jogo e)!!1)   /= 0 
              isP3Nit       = elem 2 listPlayerNit && (nitros (jogo e)!!2)   /= 0 
              isP4Nit       = elem 3 listPlayerNit && (nitros (jogo e)!!3)   /= 0 
              pic1          = picCar isP1Nit e 0
              pic2          = picCar isP2Nit e 1
              pic3          = picCar isP3Nit e 2
              pic4          = picCar isP4Nit e 3

-- | Função que recebe se o 'Carro' está a sofrer nitro ou não, o 'Estado' do jogo e o identificador do 'Carro' e devolve a 'Picture' Correspondente do carro.
picCar:: Bool -> Estado -> Int -> Picture 
picCar n e i| n         = picNitList !! i
            | otherwise = picNotNitList !! i
            where
              imagens       = images e
              picNitList    = [p1N imagens, p2N imagens, p3N imagens, p4N imagens]
              picNotNitList = [p1  imagens, p2  imagens, p3  imagens, p4  imagens]



-- | Função que lhe dando o lado da janela do ecrã e as coordenadas com o referencial no canto superior esquerdo do ecrã, as converte em coordenadas com o referencial no centro do ecrã.
cornerToCenter::Int -> Tabuleiro -> Ponto -> Ponto
cornerToCenter pSize tab (x,y) =( cx + p * x,cy - y * p) 
            where
              cy = ((fromIntegral(length tab)/2) + 0.5 )* p
              cx = (-(fromIntegral(length (head tab))/2) - 0.5) * p
              p  = fromIntegral pSize

-- *** Desenhar o mapa
-- | Função que recebe um 'Estado' e devole uma 'Picture' que corresponde ao 'Mapa' presente no 'Jogo' do 'Estado'
drawMapa:: Estado -> Picture
drawMapa e = Pictures [drawLines pSize tab, start]
            where
              (Mapa (p,o) tab) = mapa (jogo e)
              (px,py) = p
              (sx,sy) = (fromIntegral px + 0.5, fromIntegral py + 0.5)
              pSize   = sizePeca tab (winSize e)
              (x,y)   = cornerToCenter pSize tab (sx, sy) 
              start   = Translate (realToFrac x) (realToFrac y) (startLine pSize)


-- | Calcula o Tamanho ideal da Peca
sizePeca:: Tabuleiro -> (Int,Int) -> Int
sizePeca a@(l:t) (x,y)= minimum [maxX, maxY]
            where
              maxX = floor (fromIntegral y/ fromIntegral (length a))  
              maxY = floor (fromIntegral x/ fromIntegral (length l))

-- | Desenha as linhas do tabuleiro
drawLines::Int -> Tabuleiro -> Picture
drawLines s t = Pictures (placeLines s translationY (map (drawLine s) t))
            where
              translationY = (fromIntegral(length t)/2)* fromIntegral s

-- | coloca no lugar certo as linhas
placeLines:: Int -> Float -> [Picture] -> [Picture]
placeLines _ v [p]   = [Translate 0 v p]
placeLines s v (h:t) =  Translate 0 v h : placeLines s (v - fromIntegral s) t

-- | desenha uma linha de Pecas
drawLine::Int ->[Peca] -> Picture
drawLine s l = Pictures (placePeca s translationX (map (desenhaPeca s) l))
            where
              translationX = -(fromIntegral(length l)/2)* fromIntegral s

-- | coloca no lugar certo as Pecas de uma Linha
placePeca::Int -> Float -> [Picture] -> [Picture]
placePeca _ v [p]   = [Translate v 0 p]
placePeca s v (h:t) =  Translate v 0 h : placePeca s (v + fromIntegral s) t


-- | Função que converte uma 'Peca' numa 'Picture'
desenhaPeca :: Int -> Peca -> Picture
desenhaPeca s p   | typePeca p == Recta             = reta       s       
                  | typePeca p == Curva Norte       = curvaNorte s
                  | typePeca p == Curva Sul         = curvaSul   s  
                  | typePeca p == Curva Este        = curvaEste  s 
                  | typePeca p == Curva Oeste       = curvaOeste s
                  | typePeca p == Rampa Norte       = rampaNorte s
                  | typePeca p == Rampa Sul         = rampaSul   s   
                  | typePeca p == Rampa Este        = rampaEste  s
                  | typePeca p == Rampa Oeste       = rampaOeste s
                  | otherwise                       = lava

-- **** Imagem para de uma 'Peca'
-- ***** Partida
-- | Função que recebe o comprimento do lado de uma Peca e devolve a 'Picture' correspondente à linha de Partida
startLine:: Int -> Picture
startLine l = Pictures [Color white (Polygon [(-m - m/10,-r),(m + m/10,-r),(m+ m/10,r),(-m -m/10,r),(-m -m/10,-r)]),standardSquare,Translate 0 (-2*m) standardSquare,Translate 0 (-4*m) standardSquare,Translate m (-m  ) standardSquare,Translate m (-3*m) standardSquare]
            where
              m              = fromIntegral l / 5
              r              = fromIntegral l / 2
              standardSquare = Color black (Polygon [(-m,r),(0,r),(0,r-m),(-m,r-m),(-m,r)])
-- ***** Retas
-- | Função que recebe o comprimento do lado de uma recta e devolve a 'Picture' que lhe corresponde
reta :: Int -> Picture
reta l = Polygon [(-r,-r),(r,-r),(r,r),(-r,r),(-r,-r)]
            where
              r = fromIntegral l / 2

-- ***** Curvas
-- | Função que recebe o comprimento do lado de uma Peca Curva Norte e devolve a 'Picture' que lhe corresponde                
curvaNorte :: Int -> Picture
curvaNorte l = Polygon [(-r,-r),(r,-r),(r,r),(-r,-r)]
            where
              r = fromIntegral l / 2

-- | Função que recebe o comprimento do lado de uma Peca Curva Sul e devolve a 'Picture' que lhe corresponde
curvaSul :: Int -> Picture
curvaSul l = Rotate 180 (curvaNorte l)

-- | Função que recebe o comprimento do lado de uma Peca Curva Este e devolve a 'Picture' que lhe corresponde
curvaEste :: Int -> Picture
curvaEste l = Rotate 90 (curvaNorte l)

-- | Função que recebe o comprimento do lado de uma Peca Curva Oeste e devolve a 'Picture' que lhe corresponde
curvaOeste :: Int -> Picture
curvaOeste l =  Rotate 270 (curvaNorte l)

-- ***** Rampas
-- | Função que recebe o comprimento do lado de uma Peca Rampa Norte e devolve a 'Picture' que lhe corresponde
rampaNorte :: Int -> Picture
rampaNorte l = Pictures[reta l, arrow1, arrow2]
            where
              r      = fromIntegral l/2
              arrow1 = color colorarrow (Polygon [(0,0),(r, -r/2), (r,-r), (0,-r/2), (-r,-r),(-r,-r/2), (0,0)])
              arrow2 = Translate 0 r arrow1
              colorarrow = greyN 0.5

-- | Função que recebe o comprimento do lado de uma Peca Rampa Sul e devolve a 'Picture' que lhe corresponde
rampaSul :: Int -> Picture
rampaSul l = Rotate 180 (rampaNorte l)

-- | Função que recebe o comprimento do lado de uma Peca Rampa Este e devolve a 'Picture' que lhe corresponde
rampaEste :: Int -> Picture
rampaEste l = Rotate 90 (rampaNorte l)

-- | Função que recebe o comprimento do lado de uma Peca Rampa Oeste e devolve a 'Picture' que lhe corresponde
rampaOeste :: Int -> Picture
rampaOeste l =  Rotate 270 (rampaNorte l)

-- ***** Lava
-- | Função que devolve uma 'Picture' em branco
lava :: Picture
lava = blank

-- * Funções auxiliares
-- ** Sobre 'Mapa'
-- | Função que converte um 'Mapa' num 'Tabuleiro'
mapToTab::Mapa -> Tabuleiro
mapToTab (Mapa _ t) = t

-- | Função que devolve a 'Posicao' da partida de um 'Mapa' dado
mapPosInicial:: Mapa -> Posicao
mapPosInicial (Mapa (p,_) _) = p

-- | Função que devolve a 'Orientacao' da partida de um 'Mapa' dado
mapOrInicial:: Mapa -> Orientacao
mapOrInicial (Mapa (_,o) _) = o

-- ** sobre 'Color'
{- | Função que recebe o número de um Jogador e devolve a sua cor caracteristica.

== Exemplo de Utilização:
>>>colorId 0
white

>>>colorId 3
red
-}
colorId:: Int -> Color
colorId 0 = white
colorId 1 = green 
colorId 2 = blue
colorId 3 = red

-- ** Sobre 'Float'
{- | Função que recebe um 'Float' e devolve um 'Float' com o número de casas decimais especificado pelo 'Int'
== Exemplo de Utilização:

>>>rounder 3.141592654 2
3.14
-} 
rounder:: Float -> Int -> Float
rounder n i = fromIntegral(floor (n * (10 ^ i))) / (10 ^ i)

-- * Propriedades disponíveis
-- | Propriedades para mapas de terra
terra:: Propriedades
terra = Propriedades 2 3 4 2 15 180

-- | Propriedades para mapas de gelo
gelo:: Propriedades
gelo = Propriedades 0.3 0.4 2 1.5 15 270

-- | Propriedades para mapas de asfalto
asfalto:: Propriedades
asfalto = Propriedades 4 4 8 4 10 140

-- * Mapas Disoníveis
-- ** Mapa vazio
-- | Mapa sem Pecas dentro
mapaEmpty::Mapa
mapaEmpty = Mapa ((0,0),Norte) []

-- ** Mapas de asfalto
-- | Mapa de Asfalto de menor dificuldade
asfaltoEasy::Mapa
asfaltoEasy = Mapa ((3,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                                ,[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Este) 1,Peca Recta 2,Peca (Curva Este) 2,Peca Lava 0]
                                ,[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 2,Peca (Curva Sul) 2,Peca Lava 0]
                                ,[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Este) 1,Peca Recta 2,Peca (Curva Sul) 2,Peca Lava 0,Peca Lava 0]
                                ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | Mapa de Asfalto de dificuldade intermédia
asfaltoMedium::Mapa
asfaltoMedium = Mapa ((13,18),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 3,Peca Recta 3,Peca Recta 3,Peca Recta 3,Peca Recta 3,Peca (Curva Este) 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca (Curva Norte) 3,Peca Recta 3,Peca Recta 3,Peca Recta 3,Peca (Rampa Este) 3,Peca (Rampa Oeste) 3,Peca Recta 3,Peca Recta 3,Peca (Curva Sul) 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca (Rampa Norte) 2,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 3,Peca (Rampa Este) 3,Peca (Rampa Oeste) 3,Peca Recta 3,Peca Recta 3,Peca (Curva Este) 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca (Curva Oeste) 2,Peca (Curva Este) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 2,Peca (Curva Este) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 2,Peca (Curva Este) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                     [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 2,Peca Recta 2,Peca Recta 2,Peca (Rampa Este) 2,Peca Recta 3,Peca Recta 3,Peca Recta 3,Peca Recta 3,Peca Recta 3,Peca Recta 3,Peca Recta 3,Peca (Rampa Este) 3,Peca Recta 4,Peca (Curva Este) 4,Peca Lava 0],
                                     [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 4,Peca Lava 0],
                                     [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 4,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 4,Peca Lava 0],
                                     [Peca Lava 0,Peca (Curva Norte) 2,Peca Recta 2,Peca Recta 2,Peca (Rampa Este) 2,Peca (Rampa Este) 3,Peca Recta 4,Peca Recta 4,Peca Recta 4,Peca (Curva Sul) 4,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 4,Peca Lava 0],
                                     [Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 4,Peca Lava 0],
                                     [Peca Lava 0,Peca (Curva Oeste) 2,Peca Recta 2,Peca (Rampa Oeste) 1,Peca Recta 1,Peca (Rampa Este) 1,Peca Recta 2,Peca Recta 2,Peca (Rampa Este) 2,Peca Recta 3,Peca (Rampa Este) 3,Peca Recta 4,Peca Recta 4,Peca Recta 4,Peca Recta 4,Peca Recta 4,Peca Recta 4,Peca (Curva Sul) 4,Peca Lava 0],
                                     [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | Mapa de Asfalto de maior dificuldade
asfaltoHard::Mapa
asfaltoHard = Mapa ((6,4),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 2,Peca (Rampa Oeste) 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca (Curva Norte) 2,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca (Curva Este) 2,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Rampa Este) 1,Peca Recta 2,Peca Recta 2,Peca (Rampa Este) 2,Peca (Rampa Oeste) 2,Peca Recta 2,Peca (Curva Este) 2,Peca Lava 0],
                                 [Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 2,Peca (Curva Este) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],
                                 [Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 3,Peca (Curva Norte) 4,Peca Recta 4,Peca (Curva Este) 4,Peca Lava 0,Peca (Curva Oeste) 2,Peca (Curva Este) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 2,Peca (Curva Sul) 2,Peca Lava 0],
                                 [Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 4,Peca Recta 4,Peca Lava 0,Peca Recta 4,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 4,Peca Recta 4,Peca Recta 4,Peca (Curva Sul) 4,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca (Curva Oeste) 2,Peca Recta 2,Peca (Rampa Este) 2,Peca Recta 3,Peca (Rampa Este) 3,Peca (Curva Sul) 4,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 3,Peca (Curva Norte) 2,Peca (Curva Este) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 4,Peca Recta 2,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 4,Peca Recta 2,Peca (Rampa Norte) 1,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 3,Peca (Rampa Sul) 2,Peca (Rampa Norte) 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Recta 3,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 3,Peca (Curva Sul) 3,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- ** Mapas de Terra
-- | Mapa de Terra de menor dificuldade
terraEasy::Mapa
terraEasy = Mapa ((2,7),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 1,Peca Lava 0],
                               [Peca Lava 0,Peca (Rampa Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Oeste) 2,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca (Curva Sul) 2,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | Mapa de Terra de dificuldade intermédia
terraMedium::Mapa
terraMedium = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                                ,[Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                                ,[Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca (Curva Oeste) 1,Peca (Curva Este) 1,Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0]
                                ,[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Este) 1,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0]
                                ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0]
                                ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                                ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                                ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | Mapa de Terra de maior dificuldade
terraHard::Mapa
terraHard = Mapa ((6,5),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Norte) 4,Peca Recta 4,Peca Recta 4,Peca Recta 4,Peca (Curva Este) 4,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 4,Peca Lava 0,Peca (Curva Norte) 4,Peca (Curva Este) 4,Peca Recta 4,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Oeste) 4,Peca Recta 4,Peca Recta 4,Peca (Curva Sul) 4,Peca (Rampa Sul) 4,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 3,Peca Lava 0,Peca (Rampa Norte) 4,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca (Curva Oeste) 4,Peca Recta 4,Peca (Rampa Este) 4,Peca Recta 5,Peca (Rampa Este) 5,Peca Recta 6,Peca (Rampa Este) 6,Peca Recta 7,Peca (Rampa Este) 7,Peca Recta 8,Peca (Rampa Este) 8,Peca Recta 9,Peca (Rampa Este) 9,Peca Recta 10,Peca (Rampa Este) 10,Peca (Curva Este) 11,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 3,Peca Recta 3,Peca Recta 3,Peca Recta 3,Peca (Rampa Oeste) 2,Peca Recta 2,Peca (Curva Este) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 4,Peca (Rampa Este) 4,Peca (Rampa Este) 5,Peca (Rampa Este) 6,Peca (Rampa Este) 7,Peca (Rampa Este) 8,Peca (Rampa Este) 9,Peca (Rampa Este) 10,Peca (Curva Sul) 11,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 3,Peca (Curva Sul) 3,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 2,Peca (Curva Este) 2,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 4,Peca Recta 4,Peca (Rampa Oeste) 3,Peca (Rampa Oeste) 2,Peca (Rampa Oeste) 1,Peca (Rampa Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2,Peca (Curva Norte) 2,Peca (Curva Este) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca (Curva Oeste) 2,Peca Recta 2,Peca (Rampa Oeste) 1,Peca Recta 1,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 2,Peca Recta 2,Peca Recta 2,Peca (Curva Sul) 2,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- ** Mapas de gelo
-- | Mapa de Gelo de menor dificuldade
geloEasy::Mapa
geloEasy = Mapa ((4,3),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                             ,[Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                             ,[Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                             ,[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0]
                             ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Recta 1,Peca Lava 0]
                             ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0]
                             ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | Mapa de Gelo de dificuldade intermédia
geloMedium::Mapa
geloMedium = Mapa ((4,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                [Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 4,Peca Recta 4,Peca (Rampa Oeste) 3,Peca Recta 3,Peca (Curva Este) 3,Peca Lava 0],
                                [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0],
                                [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 2,Peca Lava 0,Peca (Curva Norte) 3,Peca Recta 3,Peca (Curva Sul) 3,Peca Lava 0],
                                [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                [Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                [Peca Lava 0,Peca (Curva Norte) 1,Peca (Curva Sul) 1,Peca Lava 0,Peca (Rampa Norte) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                [Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Este) 1,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

   
-- | Mapa de Gelo de maior dificuldade
geloHard::Mapa
geloHard = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                             ,[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca (Curva Este) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                             ,[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca (Curva Oeste) 1,Peca (Curva Este) 1,Peca (Curva Norte) 1,Peca (Curva Este) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                             ,[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                             ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca (Curva Oeste) 1,Peca (Curva Este) 1,Peca (Curva Norte) 1,Peca (Curva Este) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                             ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                             ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca (Curva Oeste) 1,Peca (Curva Este) 1,Peca (Curva Norte) 1,Peca (Curva Este) 1,Peca Lava 0]
                             ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0]
                             ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0]
                             ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
