{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad.State(evalStateT)
-- import Control.Monad(forever)

import Lib
import TinyUrl
import Bot

main :: IO ()
main = runBot
