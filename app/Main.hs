{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib(runBotInteractive,messageHandler)

main :: IO ()
main = runBotInteractive messageHandler
