module UrlShortener where

import Google
import Bitly
import Qps

--Тип сервиса
data Service = Google | Bitly | Qps

--Сократитель ссылок
shortUrl :: Service -> String -> IO (Either String String)
shortUrl service longUrl = case service of
                            Google -> google longUrl
                            Bitly  -> bitly  longUrl
                            Qps    -> qps    longUrl