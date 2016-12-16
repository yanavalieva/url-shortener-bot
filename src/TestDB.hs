{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE FlexibleInstances #-} 

module TestDB where 

import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.Reader.Class
import System.Environment
import Control.Monad.IO.Class
import Data.Int 
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader
import GHC.Generics (Generic) 
import Scheme

data Config = Config { connections :: ConnectionPool }

newtype App a = App { 
    runApp :: ReaderT Config IO a 
    } deriving (Functor, Applicative, Monad, 
                MonadIO, MonadReader Config )

createPool :: IO ConnectionPool
createPool = runStdoutLoggingT $ createSqlitePool "test.db3" 10

runMigrations :: SqlPersistT IO ()
runMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks connections
    liftIO $ runSqlPool query pool

createUser :: MonadIO m => Int64 -> String -> ReaderT SqlBackend m Int64
createUser id def_serv = do
    newUser <- insert $ User id def_serv
    return $ fromSqlKey newUser

main = do
    pool <- createPool
    let cfg = Config { connections = pool }
    runSqlPool runMigrations pool
    d <- runReaderT (runDb $ createUser 2 "google") cfg
    return d


