{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module MessageHandler
    ( messageHandler
    , UserCommand
    ) where

import Data.Text(Text)
import Data.Text as T(words)
import Data.Text.Read(decimal)
import Data.String(fromString)
import Data.Either(isRight)
import Data.Maybe(fromMaybe)
import Data.String(fromString)
import Control.Applicative((<|>))
import Control.Monad

data UserCommand = Help |
                   ShortUrl Text |
                   GenPrime Int |
                   Unknown Text


messageHandler :: Text -> IO Text
messageHandler "0" = return $ fromString $ replicate 100000 'A'
messageHandler  t = processCommand $ parseCommand t


factors n = [x | x <- [1..n], n `mod` x == 0]
isPrime n = factors n == [1, n]
genPrime n = (1:filter isPrime [2..]) !! n

parseCommand :: Text -> UserCommand
parseCommand t = let (cmd:args) = T.words t
                  in orUnknown (lookup cmd cmds <*> Just args)
  where
    orUnknown = fromMaybe $ Unknown t
    cmds :: [(Text, [Text] -> UserCommand)]
    cmds = [("/p", orUnknown . parseGenPrime)]
    parseGenPrime a = do
                        guard (length a == 1)
                        let p = decimal $ head a
                        guard (isRight p)
                        let Right (n,_) = p
                        return $ GenPrime n

processCommand :: UserCommand -> IO Text
processCommand (GenPrime n) = return $ fromString $
                                concat ["The ", show n, "th prime number is ", show (genPrime n)]
processCommand _ = return $ ":-)"
