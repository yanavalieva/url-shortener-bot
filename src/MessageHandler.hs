{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module MessageHandler
    ( messageHandler
    , parseCommand
    , UserCommand
    ) where

import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Read(decimal)
import Data.Either(isRight)
import Data.Maybe(fromMaybe, listToMaybe)
import Data.String(fromString)
import Control.Applicative((<|>))
import Control.Monad

import Service
data UserCommand = Help |
                   ShortUrl Service Text |
                   GenPrime Int |
                   Unknown Text
                deriving(Show)

messageHandler :: Text -> IO Text
messageHandler = processCommand . parseCommand

genPrime = (!!) (1:filter isPrime [2..])
  where
    factors n = [x | x <- [1..n], n `mod` x == 0]
    isPrime n = factors n == [1, n]

parseCommand :: Text -> UserCommand
parseCommand t = let (cmd:args) = T.words t
                 in orUnknown $ join (lookup cmd cmds <*> Just args)
  where
    orUnknown = fromMaybe $ Unknown t
    cmds :: [(Text, [Text] -> Maybe UserCommand)]
    cmds = [ ("/p",      parseGenPrimeCmd)
           , ("/bitly",  parseShortCmd Bitly)
           , ("/google", parseShortCmd Google)
           , ("/qps",    parseShortCmd Qps)
           , ("/help",   const (Just Help))
           ]
    parseGenPrimeCmd a = GenPrime <$> (decimal <$> listToMaybe a >>= fmap fst . eitherToMaybe)
    parseShortCmd s a = ShortUrl s <$> listToMaybe a

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

processCommand :: UserCommand -> IO Text
processCommand (ShortUrl s u) = return u
processCommand (Unknown t) = return $ T.concat ["Wrong cmd: `", t, "`"]
processCommand Help = return "How I can help u?"
processCommand (GenPrime n) = return $ fromString $
                                concat ["The ", show n, "th prime number is ", show $ genPrime n]
