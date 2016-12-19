module UrlShortener where

import UrlShort.Google
import UrlShort.Bitly
import UrlShort.Qps
import Service

--Сократитель ссылок
shortUrl :: Service -> String -> IO (Either String String)
shortUrl service longUrl = case service of
                            Google -> google longUrl
                            Bitly  -> bitly  longUrl
                            Qps    -> qps    longUrl