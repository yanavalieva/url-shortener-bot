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
{- getShortUrl :: MonadIO m => Int64 -> Service -> Text -> Config -> m Text
getShortUrl id service url cfg = do
    hist <- findInHistory id service url cfg
    if (null hist) then 
        return $ head hist
    else do
        short <- shortUrl service url
        case short of 
        Right sh -> do
        	createHistoryRecord id service url sh cfg
        	eturn sh
		Left e -> return e -}

-- запрос с использованием сервиса по умолчанию
{- getByDefault id url cfg = do
	serv <- createOrFindUser id cfg
	return $ getShortUrl id serv url cfg -}

-- установка сервиса по умолчанию
setDefaultService :: MonadIO m => Int64 -> Service -> Config -> m ()
setDefaultService = setNewDefaultService