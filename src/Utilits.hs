module Utilits where

import Service
import DataBase.Requests
import UrlShortener
import Control.Monad
import Control.Monad.IO.Class
import Data.Int 
import Data.Text hiding (map, head, null)
import Data.List

-- если запрос уже выполнялся, ищет его в истории, иначе генерирует новый url
getShortUrl :: Int64 -> Service -> Text -> Config -> IO Text
getShortUrl id service url cfg = do
    hist <- findInHistory id service url cfg
    if (not $ null hist) then 
        return $ head hist
    else do
        short <- shortUrl service url
        case short of 
            Right sh ->
                createHistoryRecord id service url sh cfg >>
                return sh
            Left err -> return err

-- запрос с использованием сервиса по умолчанию
getByDefault :: Int64 -> Text -> Config -> IO Text
getByDefault id url cfg = do
    serv <- createOrFindUser id cfg
    url <- getShortUrl id serv url cfg
    return url

-- установка сервиса по умолчанию
setDefaultService :: Int64 -> Service -> Config -> IO ()
setDefaultService = setNewDefaultService