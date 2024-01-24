module Controller where


import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Model
import Paddle


handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Char 's') Down _ _) gs = gs {keypresses = KeyS : keypresses gs}
handleEvent (EventKey (Char 's')   Up _ _) gs = gs {keypresses = delete KeyS (keypresses gs)}
handleEvent (EventKey (Char 'w') Down _ _) gs = gs {keypresses = KeyW : keypresses gs}
handleEvent (EventKey (Char 'w')   Up _ _) gs = gs {keypresses = delete KeyW (keypresses gs)}
handleEvent _                              gs = gs    


update :: Float -> GameState -> GameState
update delta gs = gs {
    paddles = map boundPaddle [
        movePlayer delta (keypresses gs) $ head (paddles gs),
        last (paddles gs)
    ]
}
