{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Scheme where 


import Database.Persist.Quasi
import Database.Persist.Sqlite
import ClassyPrelude.Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] 
	$(persistFileWith lowerCaseSettings "scheme")