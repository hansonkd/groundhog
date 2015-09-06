{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Database.Groundhog.TH
import Database.Groundhog.Sqlite
import Data.Text (Text)

data Person = Person {name :: Text, age :: Int, height :: Int} deriving (Eq, Show)

mkPersist defaultCodegenConfig [groundhog|
- entity: Person
|]

-- First, normal build, then profiling build with "-osuf p_o -hisuf p_hi"
main :: IO ()
main = withSqliteConn ":memory:" $ runDbConn $ do
--main = withPostgresqlConn "dbname=test user=test password=test host=localhost" $ runPostgresqlConn $ do
  runMigration $ migrate (undefined :: Person)
  let person = Person "abc" 22 180
  k <- insert $ person
  liftIO $ print k
  b <- get k
  liftIO $ print b
  -- Do some math
  result <- foldM (\acc a -> get k (\p -> return $ acc + (age p)))
  sum foldMreplicateM_ 100000 $ get k --4.3
--  replicateM_ 10000 $ select $ AgeField ==. (22 :: Int)
--  replicateM_ 100000 $ insert person
