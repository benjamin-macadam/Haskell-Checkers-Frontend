module Main where

--import Tui
import Checkers

main :: IO ()
main = human (\x z -> z) initialGameState
