{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataBase.Scheme where 


import Database.Persist.Quasi
import ClassyPrelude.Yesod
import qualified Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                    share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] 
	$(persistFileWith lowerCaseSettings "src/DataBase/scheme")