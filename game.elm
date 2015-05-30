import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import List exposing (..)
import Text exposing (fromString)
import Keyboard

type alias GameState = { x : Float, state : PlayerState, score : Int }
type PlayerState = Playing | Dead
type alias TunnelPiece = (Float,Float)

initialState : GameState
initialState = { x = 0, state = Playing, score = 0 }

grandezzaPezzo = 20
minGrandezzaTunnel = 120
larghezzaGioco = 400
altezzaGioco = 400
playerSpeedX = 14
--coloremuri _ = charcoal
coloremuri x = hsl (degrees x*2) 2 0.7
--coloremuri x = rgb (round (x*5)) (round (x*2)) 128

combine2 : Signal a -> Signal b -> Signal (a,b)
combine2 x y = let f a b = (a,b)
               in Signal.map2 f x y


combine3 x y z = let f a b c = (a,b,c)
                 in Signal.map3 f x y z

--changes 1
--lambda
--combine2 x y = Signal.map2 (\a b -> (a,b)) x y
--curried
--combine2 = Signal.map2 (\a b -> (a,b))

--changes 2
--curried
--combine2 = let f a b = (a,b)
--           in Signal.map2 f

--lambda
--combine2 = Signal.map2 (\x y -> (x,y))

stepper : (Time, Bool) -> Time -> Time
stepper (td, k) t = if k then 0 else t + td

main =
  let
    td = fps 30 --time delta - every 1/30 s
    spK = Signal.sampleOn td Keyboard.space --user input - space key
    arrK = Signal.sampleOn td Keyboard.arrows --user input - arrows
    t = Signal.foldp stepper 0 (combine2 td spK) --time since restart
    t' = Signal.map (\x -> x^1.05) t --speeds up the game
    tunB = Signal.map tunnelBorders t' --the tunnel borders
    tun = Signal.map tunnel t' --the whole displayed tunnel
    player = Signal.foldp movePlayer initialState (combine3 tunB arrK spK) --mmmm... stateful
  in
    Signal.map2 campo tun player --drawing function

movePlayer (tun,arr,space) player =
  let (x',score') = if player.state==Playing then
                      ( player.x+ (toFloat arr.x)*playerSpeedX
                      , player.score+1 )
                    else
                      ( player.x
                      , player.score )
  in
    if | space -> { player | state <- Playing, x <- 0, score <- 0 } --restart
       | x' < fst tun || x' > snd tun -> { player | state <- Dead } --death
       | otherwise -> { player | x <- x', score <- score' } --playing

campo tun player = collage larghezzaGioco 400 (
    [ filled black (rect 400 400) --background color
    , toForm (image 400 400 "stars.png") --stars
    , moveY (-200) tun --the tunnel
    , ship player ] --the player
    ++ scoreText player --the score counter
    ++ gameOverText player
  )

gameOverText {state} = if | state == Dead -> [ toForm ( centered
                                                      ( Text.height 100
                                                      ( Text.color red
                                                      ( Text.bold
                                                      ( fromString "GAME\nOVER"
                                                      )))))
                                             ]
                          | otherwise -> []


--point free version
scoreText {score} = [ (  moveY 170
                      << toForm
                      << leftAligned
                      << Text.height 30
                      << Text.color white
                      << Text.bold
                      << fromString )
                      ("Score: " ++ (toString score))
                    ]

--choose the right player image based on state
ship p = move (p.x, -170) (
             if p.state==Dead then
               toForm (image 128 128 "explosion.gif")
             else
               toForm (image 32 32 "player.png")
               --(rotate (degrees -30) (filled red (ngon 3 10)))
           )

--make a drawable tunnel (the whole tunnel) given the time
tunnel t = group (map (\x -> moveY (x*grandezzaPezzo) (tunnelPieceForm (t+((x-2)*50)))) [0..20]) -- il 2 corregge la posizione sfasata del giocatore

--make a drawable tunnel piece given the time
tunnelPieceForm n = group (toRects (tunnelBorders n))

--make 2 rects given the tunnel borders
toRects (x1,x2) =
  [ moveX (-(larghezzaGioco/4) + x1/2) (filled (coloremuri x2) (rect ((larghezzaGioco/2)+x1) grandezzaPezzo))
  , moveX ( (larghezzaGioco/4) + x2/2) (filled (coloremuri x2) (rect ((larghezzaGioco/2)-x2) grandezzaPezzo)) ]

--returns the tunnel borders in a given time
tunnelBorders : Time -> TunnelPiece
tunnelBorders n = let grandezzaTunnel = max ((30000-n)/100) minGrandezzaTunnel
                      posizioneCentrale = (tunnelFunction n) *((larghezzaGioco/2)-grandezzaTunnel/2) in
    (posizioneCentrale-(grandezzaTunnel/2),posizioneCentrale+(grandezzaTunnel/2))

--must return a number from -1 to 1
tunnelFunction x' = let x = x'/400 in
  ( 2*sin(x) + sin(1.1*(x+20)) + sin(2*(x+20)) )/4
