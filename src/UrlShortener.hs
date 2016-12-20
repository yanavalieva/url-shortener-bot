module UrlShortener where

import UrlShort.Google
import UrlShort.Bitly
import UrlShort.Qps
import Service
import Data.Text (Text, pack)

--Сократитель ссылок
shortUrl :: Service -> Text -> IO (Either Text Text)
shortUrl service longUrl = case service of
                            Google -> google longUrl
                            Bitly  -> bitly  longUrl
                            Qps    -> qps    longUrl