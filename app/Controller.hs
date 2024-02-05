module Controller where


import Data.List
import Text.Printf

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Model
import Paddle


handleEventIO :: Event -> GameState -> IO GameState
handleEventIO e gs@Game{} = do
    let new_gs = handleEvent e gs
    -- print $ wait gs
    return new_gs

handleEventIO e gs@(Menu n) = do
    let new_gs = handleMenuEvent e gs
    return new_gs
    
handleEventIO e gs@(Result _) = do
    let new_gs = handleResultEvent e
    return new_gs
  where handleResultEvent (EventKey (SpecialKey KeySpace) Down _ _) = Menu 0
        handleResultEvent _ = gs


handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Char 'm') Down _ _) gs = Menu 0
handleEvent (EventKey (Char 's') Down _ _) gs = gs {keypresses = KeyS : keypresses gs}
handleEvent (EventKey (Char 's')   Up _ _) gs = gs {keypresses = delete KeyS (keypresses gs)}
handleEvent (EventKey (Char 'w') Down _ _) gs = gs {keypresses = KeyW : keypresses gs}
handleEvent (EventKey (Char 'w')   Up _ _) gs = gs {keypresses = delete KeyW (keypresses gs)}
handleEvent (EventKey (Char 'j') Down _ _) gs = gs {scores = (scores gs) {score_1 = score_1 (scores gs) + 1}}
handleEvent (EventKey (Char 'k') Down _ _) gs = gs {scores = (scores gs) {score_2 = score_2 (scores gs) + 1}}
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) gs | wait gs == 0 = gs {wait = -1}
                                                         | otherwise    = gs
handleEvent _                              gs = gs    


handleMenuEvent :: Event -> GameState -> GameState
handleMenuEvent (EventKey (Char 'w') Down _ _) (Menu 1) = Menu 0
handleMenuEvent (EventKey (Char 's') Down _ _) (Menu 0) = Menu 1
handleMenuEvent (EventKey (SpecialKey KeySpace) Down _ _) (Menu 0) = initialState
handleMenuEvent (EventKey (SpecialKey KeySpace) Down _ _) (Menu 1) = error "quit :)"
handleMenuEvent _ gs = gs


updateIO :: Float -> GameState -> IO GameState
updateIO delta gs = do
    let new_gs = update delta gs
    return new_gs


update :: Float -> GameState -> GameState
update delta gs@Game{} | wait gs > 1  = gs {wait = wait gs - 1}
                       | wait gs == 1 = gs {ball = initialBall (lastScorer gs * (-400.0)), paddles = initialPaddles, wait = 0}
                       | wait gs == 0 = gs
                       | otherwise    = checkWinner $ checkScored $ gs {
                                           paddles = map boundPaddle [
                                                movePlayer delta (keypresses gs) $ head (paddles gs),
                                                last (paddles gs)
                                            ],
                                            ball = foldl (bounceFromPaddle delta) moved (paddles gs)
                                        }
    where moved = bounceFromWall $ moveBall delta (ball gs)
          checkScored gs | scored (ball gs) == (-1) = gs {scores = (scores gs) {score_1 = score_1 (scores gs) + 1}, wait = 30, lastScorer = -1}
                         | scored (ball gs) ==   1  = gs {scores = (scores gs) {score_2 = score_2 (scores gs) + 1}, wait = 30, lastScorer =  1}
                         | otherwise                = gs
          checkWinner gs | score_1 (scores gs) >= 11 && score_2 (scores gs) < score_1 (scores gs) - 1 = Result (-1)
                         | score_2 (scores gs) >= 11 && score_1 (scores gs) < score_2 (scores gs) - 1 = Result   1
                         | otherwise                                                                  = gs
update _ gs = gs
