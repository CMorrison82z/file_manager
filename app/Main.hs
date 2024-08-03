{-# HLINT ignore "Use camelCase" #-}

module Main where

import Temp

main :: IO ()
main = get_initial_state >>= app
