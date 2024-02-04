module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Model
import View
import Controller


window :: Display
window = InWindow "Pong" (1500, 800) (10, 10)

background :: Color
background = black


-- playIO, mainly because it allows me to print values in the three main functions for debugging
-- eventually also for threading to allow audio
main :: IO ()
main = playIO window background 30 initialState render handleEventIO updateIO




-- idea for fullscreen
-- add screensize to game state
-- get the screensize in a do-block in main, like so
-- 
-- main :: IO ()
-- main = do
--     screensize <- getScreenSize
--     play window background 30 (initialState screensize) render handleEvents update
