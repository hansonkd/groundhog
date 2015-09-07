{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Database.Groundhog.TH
import Database.Groundhog.Sqlite
import Data.Text (Text, pack)

data Person = Person {name :: Text, age :: Int, height :: Int} deriving (Eq, Show)

mkPersist defaultCodegenConfig [groundhog|
- entity: Person
|]

-- First, normal build, then profiling build with "-osuf p_o -hisuf p_hi"
main :: IO ()
main = withSqlitePool ":memory:" 10 $ \gCon -> runDbConn (do { runMigration $ migrate (undefined :: Person); res <- foldM (\acc b -> go >>= return . (+) acc) 0 [1..100]; liftIO $ print res}) gCon
  where go = do
          let person = Person (pack "abc") 22 180
          k <- insert $ person
          b <- get k
          -- Do some math
          (replicateM_ 1000 $ insert person)
          replicateM_ 1000 $ get k

          rs <- selectAll
          let calc = (foldr (((+) . age . snd)) 0 rs)
          calc `seq` return calc

--  sum foldMreplicateM_ 100000 $ get k --4.3
--  replicateM_ 10000 $ select $ AgeField ==. (22 :: Int)
--  replicateM_ 100000 $ insert person
