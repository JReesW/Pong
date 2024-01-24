module Main where


import Graphics.Gloss

import Model
import View
import Controller


window :: Display
window = InWindow "Pong" (1500, 800) (10, 10)

background :: Color
background = black

main :: IO ()
main = play window background 30 initialState render handleEvent update


-- idea for fullscreen
-- add screensize to game state
-- get the screensize in a do-block in main, like so
-- 
-- main :: IO ()
-- main = do
--     screensize <- getScreenSize
--     play window background 30 (initialState screensize) render handleEvents update
