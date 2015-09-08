{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Database.Groundhog.TH
import Database.Groundhog.Sqlite
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Maybe (fromJust)
data Person = Person {name :: Text, name2 :: Text, age :: Int, height :: Int} deriving (Eq, Show)

mkPersist defaultCodegenConfig [groundhog|
- entity: Person
|]

longString :: String
longString = ("abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc abc")

-- First, normal build, then profiling build with "-osuf p_o -hisuf p_hi"
main :: IO ()
main = withSqlitePool ":memory:" 10 $ \gCon -> runDbConn (do { runMigration $ migrate (undefined :: Person); res <- foldM (\acc b -> acc `seq` go >>= return . (+) acc) 0 [1..100]; liftIO $ print res}) gCon
  where go = do
          let person = Person (pack longString) (pack longString) 22 180
          k <- insert $ person
          b <- get k
          -- Do some math
          (replicateM_ 1000] $ insert person)
          replicateM_ 1000 $ get k
          ps <- selectAllStream startStreamP
          let calc = ps `seq` (foldr (\ b acc -> let nlength = acc + (b) in  nlength `seq` (nlength)) 0 ps)
          calc `seq` return calc
        startStreamP = streamP (0 :: Int)
        streamP acc getter = do
            ment <- getter
            case ment of
                Just (_, ent) -> acc `seq` streamP (acc + ((T.length $ name2 ent <> name ent))) getter
                Nothing -> return acc
