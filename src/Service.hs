{-# LANGUAGE TemplateHaskell #-}

module Service where

import ClassyPrelude.Yesod

data Service = Google | Bitly | Qps 
    deriving (Show, Read, Eq)
derivePersistField "Service"
