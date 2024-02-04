module Controller where


import Data.List
import Text.Printf

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Model
import Paddle


handleEventIO :: Event -> GameState -> IO GameState
handleEventIO e gs = do
    let new_gs = handleEvent e gs
    return new_gs


handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Char 's') Down _ _) gs = gs {keypresses = KeyS : keypresses gs}
handleEvent (EventKey (Char 's')   Up _ _) gs = gs {keypresses = delete KeyS (keypresses gs)}
handleEvent (EventKey (Char 'w') Down _ _) gs = gs {keypresses = KeyW : keypresses gs}
handleEvent (EventKey (Char 'w')   Up _ _) gs = gs {keypresses = delete KeyW (keypresses gs)}
handleEvent (EventKey (Char 'j') Down _ _) gs = gs {scores = (scores gs) {score_1 = score_1 (scores gs) + 1}}
handleEvent (EventKey (Char 'k') Down _ _) gs = gs {scores = (scores gs) {score_2 = score_2 (scores gs) + 1}}
handleEvent _                              gs = gs    


updateIO :: Float -> GameState -> IO GameState
updateIO delta gs = do
    let new_gs = update delta gs
    return new_gs


update :: Float -> GameState -> GameState
update delta gs = gs {
    paddles = map boundPaddle [
        movePlayer delta (keypresses gs) $ head (paddles gs),
        last (paddles gs)
    ],
    ball = foldl (bounceFromPaddle delta) moved (paddles gs)
}
    where moved = bounceFromWall $ moveBall delta (ball gs)
