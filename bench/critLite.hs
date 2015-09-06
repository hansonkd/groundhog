{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, OverloadedStrings, FlexibleContexts, FlexibleInstances, RankNTypes, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Main where

import qualified Database.Groundhog.Core as G
import qualified Database.Groundhog.Generic as G
import qualified Database.Groundhog.Sqlite as G
import qualified Database.Groundhog.TH as G
import Database.Groundhog.TH (groundhog)

import Control.Monad.Logger (MonadLogger(..), NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist.TH (persistUpperCase)

--import Database.Esqueleto as E

import Criterion.Main
import Criterion.Main.Options
import Criterion.Types
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text (Text)

data GPerson = GPerson { name :: Text, age :: Int, height :: Int }
G.mkPersist (G.defaultCodegenConfig {G.migrationFunction = Just "migrateG"}) [groundhog|
- entity: GPerson
|]

myConfig = defaultConfig {
             reportFile = Just "bench.html"
           }

gPerson = GPerson "John Doe" 23 180
gCond :: G.DbDescriptor db => G.Cond db (G.RestrictionHolder GPerson GPersonConstructor)
gCond = NameField G.==. ("abc" :: Text) G.&&. AgeField G.==. (40 :: Int) G.&&. HeightField G.==. (160 :: Int)

gMigrate :: (G.PersistBackend (G.DbPersist conn m), MonadIO m) => G.DbPersist conn m () -> G.DbPersist conn m (G.Key GPerson G.BackendSpecific)
gMigrate truncate = G.runMigration migrateG >> truncate >> G.insert gPerson


instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()

-- open transaction to reduce execution time on the DB side
eachStatementInTransaction :: Bool
eachStatementInTransaction = False

-- operatons are replicated to reduce influence of running a monad on the actual library and database performance measurements
numberOfOperations :: Int
numberOfOperations = 10

main = 
  G.withSqliteConn ":memory:" $ \gConn -> do
    gKey <- G.runDbConn (gMigrate $ return ()) gConn
{-
  G.withPostgresqlConn "dbname=test user=test password=test host=localhost" $ \gConn ->
  P.withPostgresqlConn "dbname=test user=test password=test host=localhost" $ \pConn -> do
    gKey <- G.runDbConn (gMigrate $ G.executeRaw False "truncate table \"GPerson\"" []) gConn
    pKey <- runResourceT $ runNoLoggingT $ P.runSqlConn (pMigrate $ P.rawExecute "truncate table \"PPerson\"" []) pConn
-}
    unless eachStatementInTransaction $ do
      runNoLoggingT $ G.runDbPersist (G.executeRaw False "BEGIN" []) gConn

    let mkBench :: (forall m . G.PersistBackend m => m a1) -> [Benchmark]
        mkBench gm = [bench "groundhog" $ whnfIO $ runSqlite gm] where
          (runSqlite) = if eachStatementInTransaction
            then (\gm -> G.runDbConn (replicateM_ numberOfOperations gm) gConn)
            else (\gm -> G.runDbConnNoTransaction (replicateM_ numberOfOperations gm) gConn)
    defaultMainWith myConfig
      [ bgroup "get" $ mkBench (G.get gKey)
--      , bgroup "get" [bench "esqueleto" $ whnfIO $  runPers (E.select $ E.from $ \p -> E.where_ (p ^. PPersonId ==. val pKey) >> return p)]
      , bgroup "replace" $ mkBench (G.replace gKey gPerson)
      , bgroup "select" $ mkBench (G.project (G.AutoKeyField, GPersonConstructor) gCond)
      , bgroup "updateByKey" $ mkBench (G.update [NameField G.=. ("abc" :: Text)] $ G.AutoKeyField G.==. gKey)
      , bgroup "updateWhere" $ mkBench (G.update [NameField G.=. ("abc" :: Text)] gCond)
      , bgroup "count" $ mkBench (G.count gCond)
      , bgroup "deleteBy" $ mkBench (G.deleteBy gKey)
      , bgroup "deleteWhere" $ mkBench (G.delete gCond)
      , bgroup "insert" $ mkBench (G.insert gPerson)
      , bgroup "insert_" $ mkBench (G.insert_ gPerson)
      ]
