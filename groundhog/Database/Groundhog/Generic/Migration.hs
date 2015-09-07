{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- | This helper module is intended for use by the backend creators
module Database.Groundhog.Generic.Migration
  ( Column(..)
  , Reference(..)
  , QualifiedName
  , TableInfo(..)
  , UniqueDefInfo
  , AlterColumn(..)
  , AlterTable(..)
  , AlterDB(..)
  , MigrationPack(..)
  , SchemaAnalyzer(..)
  , migrateRecursively
  , migrateSchema
  , migrateEntity
  , migrateList
  , getAlters
  , defaultMigConstr
  , showReferenceAction
  , readReferenceAction
  , intercalateS
  , delim'
  ) where

import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql (flatten, mainTableName, tableName, intercalateS)

import Control.Applicative (Applicative)
import Control.Arrow ((***), (&&&))
import Control.Monad (liftM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (gets, modify)
import Data.Either (lefts)
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import qualified Data.Map as Map
import Data.List (group, intercalate)
import Data.Maybe (fromMaybe, mapMaybe)

data Column = Column
    { colName :: Utf8
    , colNull :: Bool
    , colType :: DbTypePrimitive
    , colDefault :: Maybe Utf8
    } deriving (Eq, Show)

data Reference = Reference {
    referencedTableName :: QualifiedName
  , referencedColumns :: [(Utf8, Utf8)] -- ^ child column, parent column
  , referenceOnDelete :: Maybe ReferenceActionType
  , referenceOnUpdate :: Maybe ReferenceActionType
  } deriving Show

-- | Schema-qualified name of a table of another database object
type QualifiedName = (Maybe Utf8, Utf8)

-- | Either column name or expression
type UniqueDefInfo = UniqueDef' Utf8 (Either Utf8 Utf8)
data TableInfo = TableInfo {
    tableColumns :: [Column]
  , tableUniques :: [UniqueDefInfo]
    -- | constraint name and reference
  , tableReferences :: [(Maybe Utf8, Reference)]
} deriving Show

data AlterColumn = Type DbTypePrimitive | IsNull | NotNull
                 | Default Utf8 | NoDefault | UpdateValue Utf8 deriving Show

data AlterTable = AddUnique UniqueDefInfo
                | DropConstraint Utf8
                | DropIndex Utf8
                | AddReference Reference
                | DropReference Utf8
                | DropColumn Utf8
                | AddColumn Column
                | AlterColumn Column [AlterColumn] deriving Show

data AlterDB = AddTable Utf8
             -- | Qualified table name, create statement, structure of table from DB, structure of table from datatype, alters
             | AlterTable QualifiedName Utf8 TableInfo TableInfo [AlterTable]
             -- | Qualified trigger name, qualified table name
             | DropTrigger QualifiedName QualifiedName
             -- | Qualified trigger name, qualified table name, body
             | AddTriggerOnDelete QualifiedName QualifiedName Utf8
             -- | Qualified trigger name, qualified table name, field name, body
             | AddTriggerOnUpdate QualifiedName QualifiedName (Maybe Utf8) Utf8
             -- | Statement which creates the function
             | CreateOrReplaceFunction Utf8
             -- | Qualified function name
             | DropFunction QualifiedName
             -- | Schema name, if not exists
             | CreateSchema Utf8 Bool
  deriving Show

data MigrationPack m = MigrationPack {
    compareTypes :: DbTypePrimitive -> DbTypePrimitive -> Bool
  , compareRefs :: (Maybe Utf8, Reference) -> (Maybe Utf8, Reference) -> Bool
  , compareUniqs :: UniqueDefInfo -> UniqueDefInfo -> Bool
  , compareDefaults :: Utf8 -> Utf8 -> Bool
  , migTriggerOnDelete :: QualifiedName -> [(Utf8, Utf8)] -> m (Bool, [AlterDB])
  , migTriggerOnUpdate :: QualifiedName -> [(Utf8, Utf8)] -> m [(Bool, [AlterDB])]
  , migConstr :: MigrationPack m -> EntityDef -> ConstructorDef -> m (Bool, SingleMigration)
  , escape :: Utf8 -> Utf8
  , autoincrementedKeyTypeName :: Utf8
  , mainTableId :: Utf8
  , defaultPriority :: Int
  -- | Sql pieces for the create table statement that add constraints and alterations for running after the table is created
  , addUniquesReferences :: [UniqueDefInfo] -> [Reference] -> ([Utf8], [AlterTable])
  , showSqlType :: DbTypePrimitive -> Utf8
  , showColumn :: Column -> Utf8
  , showAlterDb :: AlterDB -> SingleMigration
  , defaultReferenceOnDelete :: ReferenceActionType
  , defaultReferenceOnUpdate :: ReferenceActionType
}



mkColumns :: DbTypePrimitive -> (Utf8, DbType) -> ([Column] -> [Column])
mkColumns autoKeyType field = go "" field where
  go prefix (fname, typ) acc = (case typ of
    DbEmbedded (EmbeddedDef flag ts) _ -> foldr (go prefix') acc (map (\(a, b)  -> (textToUtf8 a, b)) ts) where prefix' = if flag then "" else prefix <> fname <> delim'
    DbList _ _ -> Column (fullName) False autoKeyType Nothing : acc
    DbTypePrimitive t nullable def _ -> Column (fullName) nullable t (fmap textToUtf8 def) : acc) where
      fullName = prefix <> fname :: Utf8

mkReferences :: DbTypePrimitive -> (Utf8, DbType) -> [Reference]
mkReferences autoKeyType field = concat $ traverseDbType f field where
  f (DbEmbedded _ ref) cols = maybe [] (return . mkReference (map utf8ToText cols)) ref
  f (DbList lName _) cols = [mkReference (map utf8ToText cols) (Right ((Nothing, lName), ["id"]), Nothing, Nothing)]
  f (DbTypePrimitive _ _ _ ref) cols = maybe [] (return . mkReference (map utf8ToText cols)) ref
  mkReference :: [Text] -> ParentTableReference -> Reference
  mkReference cols (parent, onDel, onUpd) = case parent of
    Left (e, Nothing) -> Reference (fmap textToUtf8 $ entitySchema e, textToUtf8 $ entityName e) (zipWith' ((,) . textToUtf8) cols [textToUtf8 keyName]) onDel onUpd where
      keyName = case constructors e of
        [cDef] -> fromMaybe (error "mkReferences: autokey name is Nothing") $ constrAutoKeyName cDef
        _      -> "id"
    Left (e, (Just uName)) -> Reference (fmap textToUtf8 $ entitySchema e, textToUtf8 $ entityName e) (zipWith' ((,) . textToUtf8) cols (map (colName) parentColumns)) onDel onUpd where
      cDef = case constructors e of
        [cDef'] -> cDef'
        _       -> error "mkReferences: datatype with unique key cannot have more than one constructor"
      uDef = findOne "unique" uniqueDefName (Just uName) $ constrUniques cDef
      fields = map (\(fName, _) -> findOne "field" fst fName $ constrParams cDef) $ getUniqueFields uDef
      parentColumns = foldr (\(a, b) acc -> mkColumns autoKeyType (textToUtf8 a, b) acc) [] fields
    Right (parentTable, parentColumns) -> Reference ( (\(f, s) -> (fmap textToUtf8 f, textToUtf8 s)) parentTable) (zipWith' ((,) . textToUtf8) cols (map textToUtf8 parentColumns)) onDel onUpd
  zipWith' _ [] [] = []
  zipWith' g (x:xs) (y:ys) = g x y: zipWith' g xs ys
  zipWith' _ _ _ = error "mkReferences: the lists have different length"

traverseDbType :: (DbType -> [Utf8] -> a) -> (Utf8, DbType) -> [a]
traverseDbType f field = snd $ go "" field where
  go prefix (fname, typ) = (case typ of
    t@(DbEmbedded (EmbeddedDef flag ts) _) -> (cols, f t cols : xs) where
      prefix' = if flag then "" else prefix <> fname <> delim'
      (cols, xs) = concatMap' (go prefix') (map (\(a, b) -> (textToUtf8 a, b)) ts)
    t@(DbList _ _) -> ([name], [f t [name]])
    t@(DbTypePrimitive _ _ _ _) -> ([name], [f t [name]])) where
      name = prefix <> fname
      concatMap' g xs = concat *** concat $ unzip $ map g xs

-- | Create migration for a given entity and all entities it depends on.
-- The stateful Map is used to avoid duplicate migrations when an entity type
-- occurs several times in a datatype
migrateRecursively :: (PersistBackend m, PersistEntity v) => 
     (Text    -> m SingleMigration)    -- ^ migrate schema
  -> (EntityDef -> m SingleMigration) -- ^ migrate entity
  -> (DbType    -> m SingleMigration) -- ^ migrate list
  -> v                                -- ^ initial entity
  -> Migration m
migrateRecursively migS migE migL v = result where
  result = migEntity $ entityDef proxy v
  proxy = (undefined :: t m a -> proxy (PhantomDb m)) result
  go l@(DbList lName t) = f ("list " <> lName) (migL l) (go t)
  go (DbEmbedded (EmbeddedDef _ ts) ref) = mapM_ (go . snd) ts >> migRef ref
  go (DbTypePrimitive _ _ _ ref) = migRef ref
  allSubtypes = map snd . concatMap constrParams . constructors
  migRef ref = case ref of
    Just (Left (e, _), _, _) -> migEntity e
    _ -> return ()
  migEntity e = do
    case entitySchema e of
      Just name -> f ("schema " <> name) (migS name) (return ())
      Nothing -> return ()
    f ("entity " <> (utf8ToText $ mainTableName id e)) (migE e) (mapM_ go (allSubtypes e))
  f name mig cont = do
    a <- gets (Map.lookup name)
    when (a == Nothing) $
      lift mig >>= modify . Map.insert name >> cont

migrateSchema :: SchemaAnalyzer m => MigrationPack m -> Utf8 -> m SingleMigration
migrateSchema MigrationPack{..} schema = do
  x <- schemaExists schema
  return $ if x
    then Right []
    else showAlterDb $ CreateSchema schema False

migrateEntity :: (SchemaAnalyzer m, PersistBackend m) => MigrationPack m -> EntityDef -> m SingleMigration
migrateEntity m@MigrationPack{..} e = do
  autoKeyType <- fmap getAutoKeyType phantomDb
  let name = entityName e
      constrs = constructors e
      mainTableQuery = "CREATE TABLE " <> escape (textToUtf8 name) <> " (" <> mainTableId <> " " <> autoincrementedKeyTypeName <> ", discr INTEGER NOT NULL)"
      expectedMainStructure = TableInfo [Column "id" False autoKeyType Nothing, Column "discr" False DbInt32 Nothing] [UniqueDef Nothing (UniquePrimary True) [Left "id"]] []

  if isSimple constrs
    then do
      x <- analyzeTable (fmap textToUtf8 $ entitySchema e, textToUtf8 name)
      -- check whether the table was created for multiple constructors before
      case x of
        Just old | null $ getAlters m old expectedMainStructure -> return $ Left
          ["Datatype with multiple constructors was truncated to one constructor. Manual migration required. Datatype: " <> name]
        _ -> liftM snd $ migConstr m e $ head constrs
    else do
      mainStructure <- analyzeTable (fmap textToUtf8 $ entitySchema e, textToUtf8 name)
      let constrTable c = name <> delim <> constrName c
      res <- mapM (migConstr m e) constrs
      return $ case mainStructure of
        Nothing -> 
          -- no constructor tables can exist if there is no main data table
          let orphans = filter (fst . fst) $ zip res constrs
          in if null orphans
            then mergeMigrations $ Right [(False, defaultPriority, (mainTableQuery))]:map snd res
            else Left $ map (\(_, c) -> "Orphan constructor table found: " <> constrTable c) orphans
        Just mainStructure' -> if null $ getAlters m mainStructure' expectedMainStructure
          then let
               -- the datatype had also many constructors before
               -- check whether any new constructors appeared and increment older discriminators, which were shifted by newer constructors inserted not in the end
               updateDiscriminators = go 0 . map (head &&& length) . group . map fst $ res where
                  go acc ((False, n):(True, n2):xs) = (False, defaultPriority, "UPDATE " <> escape (textToUtf8 name) <> " SET discr = discr + " <> (textToUtf8 $ T.pack $ show n) <> " WHERE discr >= " <>  (textToUtf8 $ T.pack $ show acc)) : go (acc + n + n2) xs
                  go acc ((True, n):xs) = go (acc + n) xs
                  go _ _ = []
               in mergeMigrations $ Right updateDiscriminators: map snd res
          else Left ["Unexpected structure of main table for Datatype: " <> name <> ". Table info: " <> (T.pack $ show mainStructure')]

migrateList :: (SchemaAnalyzer m, PersistBackend m) => MigrationPack m -> DbType -> m SingleMigration
migrateList m@MigrationPack{..} (DbList mainName t) = do
  autoKeyType <- fmap getAutoKeyType phantomDb
  let valuesName = mainName <> delim <> "values"
      (valueCols, valueRefs) = (($ []) . mkColumns autoKeyType) &&& mkReferences autoKeyType $ ("value", t)
      refs' = Reference (Nothing, textToUtf8 mainName) [("id", "id")] (Just Cascade) Nothing : valueRefs
      expectedMainStructure = TableInfo [Column "id" False autoKeyType Nothing] [UniqueDef Nothing (UniquePrimary True) [Left "id"]] []
      mainQuery = "CREATE TABLE " <> escape (textToUtf8 mainName) <> " (id " <> autoincrementedKeyTypeName <> ")"
      (addInCreate, addInAlters) = addUniquesReferences [] refs'
      expectedValuesStructure = TableInfo valueColumns [] (map (\x -> (Nothing, x)) refs')
      valueColumns = Column "id" False autoKeyType Nothing : Column "ord" False DbInt32 Nothing : valueCols
      valuesQuery = "CREATE TABLE " <> escape (textToUtf8 valuesName) <> " (" <> (intercalateS ", " (map showColumn valueColumns <> addInCreate)) <> ")"
  -- TODO: handle case when outer entity has a schema
  mainStructure <- analyzeTable (Nothing, textToUtf8 mainName)
  valuesStructure <- analyzeTable (Nothing, (textToUtf8 valuesName))
  let triggerMain = []
  (_, triggerValues) <- migTriggerOnDelete (Nothing, (textToUtf8 valuesName)) $ mkDeletes escape ("value", t)
  return $ case (mainStructure, valuesStructure) of
    (Nothing, Nothing) -> let
      rest = [AlterTable (Nothing, (textToUtf8 valuesName)) valuesQuery expectedValuesStructure expectedValuesStructure addInAlters]
      in mergeMigrations $ map showAlterDb $ [AddTable mainQuery, AddTable valuesQuery] ++ rest ++ triggerMain ++ triggerValues
    (Just mainStructure', Just valuesStructure') -> let
      f name a b = if null $ getAlters m a b
        then []
        else ["List table " <> name <> " error. Expected: " <> (T.pack $ show b) <> ". Found: " <> (T.pack $ show a)]
      errors = f mainName mainStructure' expectedMainStructure ++ f valuesName valuesStructure' expectedValuesStructure
      in if null errors then Right [] else Left errors
    (_, Nothing) -> Left ["Found orphan main list table " <> mainName]
    (Nothing, _) -> Left ["Found orphan list values table " <> valuesName]
migrateList _ t = fail $ "migrateList: expected DbList, got " <> show t

getAlters :: MigrationPack m
          -> TableInfo -- ^ From database
          -> TableInfo -- ^ From datatype
          -> [AlterTable]
getAlters m@MigrationPack{..} (TableInfo oldColumns oldUniques oldRefs) (TableInfo newColumns newUniques newRefs) = tableAlters
  where
    (oldOnlyColumns, newOnlyColumns, commonColumns) = matchElements ((==) `on` colName) oldColumns newColumns
    (oldOnlyUniques, newOnlyUniques, commonUniques) = matchElements compareUniqs oldUniques newUniques
    (oldOnlyRefs, newOnlyRefs, _) = matchElements compareRefs oldRefs newRefs
    primaryColumns = lefts $ concatMap uniqueDefFields $ filter ((== UniquePrimary True) . uniqueDefType) oldUniques

    colAlters = mapMaybe (\(a, b) -> mkAlterColumn b $ migrateColumn m a b) (filter ((`notElem` primaryColumns) . colName . fst) commonColumns)
    mkAlterColumn col alters = if null alters then Nothing else Just $ AlterColumn col alters
    tableAlters = 
         map (DropColumn . colName) oldOnlyColumns
      ++ map AddColumn newOnlyColumns
      ++ colAlters
      ++ map dropUnique oldOnlyUniques
      ++ map AddUnique newOnlyUniques
      ++ concatMap (uncurry migrateUniq) commonUniques
      ++ map (DropReference . fromMaybe (error "getAlters: old reference does not have name") . fst) (oldOnlyRefs)
      ++ map (AddReference . snd) newOnlyRefs

-- from database, from datatype
migrateColumn :: MigrationPack m -> Column -> Column -> [AlterColumn]
migrateColumn MigrationPack{..} (Column _ isNull1 type1 def1) (Column _ isNull2 type2 def2) = modDef ++ modNull ++ modType where
  modNull = case (isNull1, isNull2) of
    (False, True) -> [IsNull]
    (True, False) -> case def2 of
      Nothing -> [NotNull]
      Just s -> [UpdateValue s, NotNull]
    _ -> []
  modType = if compareTypes type1 type2 then [] else [Type type2]
  modDef = case (def1, def2) of
    (Nothing, Nothing) -> []
    (Just def1', Just def2') | compareDefaults def1' def2' -> []
    _ -> [maybe NoDefault Default def2]

-- from database, from datatype
migrateUniq :: UniqueDefInfo -> UniqueDefInfo -> [AlterTable]
migrateUniq u1@(UniqueDef _ _ cols1) u2@(UniqueDef _ _ cols2) = if haveSameElems (==) cols1 cols2
  then []
  else [dropUnique u1, AddUnique u2]

dropUnique :: UniqueDefInfo -> AlterTable
dropUnique (UniqueDef name typ _) = (case typ of
  UniqueConstraint -> DropConstraint name'
  UniqueIndex -> DropIndex name'
  UniquePrimary _ -> DropConstraint name') where
  name' = fromMaybe (error $ "dropUnique: constraint which should be dropped does not have a name") name
  
defaultMigConstr :: (SchemaAnalyzer m, PersistBackend m) => MigrationPack m -> EntityDef -> ConstructorDef -> m (Bool, SingleMigration)
defaultMigConstr m@MigrationPack{..} e constr = do
  let simple = isSimple $ constructors e
      name = entityName e
      qualifiedCName = (fmap textToUtf8  (entitySchema e), textToUtf8 $ if simple then name else name <> delim <> constrName constr)
  autoKeyType <- fmap getAutoKeyType phantomDb
  tableStructure <- analyzeTable qualifiedCName
  let dels = concatMap (mkDeletes escape) $ (map (\(a, b) -> (textToUtf8 a, b)) (constrParams constr))
  (triggerExisted, delTrigger) <- migTriggerOnDelete qualifiedCName dels
  updTriggers <- liftM (concatMap snd) $ migTriggerOnUpdate qualifiedCName dels
  
  let (expectedTableStructure, (addTable, addInAlters)) = (case constrAutoKeyName constr of
        Nothing -> (TableInfo columns uniques (mkRefs refs), f [] columns uniques refs)
        Just keyName -> let keyColumn = Column (textToUtf8 keyName) False autoKeyType Nothing
                        in if simple
          then (TableInfo (keyColumn:columns) (uniques ++ [UniqueDef Nothing (UniquePrimary True) [Left (textToUtf8 keyName)]]) (mkRefs refs)
               , f [escape (textToUtf8 keyName) <> " " <> (autoincrementedKeyTypeName)] columns uniques refs)
          else let columns' = keyColumn:columns
                   refs' = refs ++ [Reference (fmap textToUtf8 $ entitySchema e, textToUtf8 name) [(textToUtf8 keyName, mainTableId)] (Just Cascade) Nothing]
                   uniques' = uniques ++ [UniqueDef Nothing UniqueConstraint [Left (textToUtf8 keyName)]]
               in (TableInfo columns' uniques' (mkRefs refs'), f [] columns' uniques' refs')) where
        (columns, refs) = foldr (mkColumns autoKeyType) [] &&& concatMap (mkReferences autoKeyType) $ (map (\(a, b) -> (textToUtf8 a, b)) (constrParams constr))
        uniques = map (\u -> u {uniqueDefName = (fmap textToUtf8 (uniqueDefName u)), uniqueDefFields = concatMap (either (map Left . ($ []) . flatten id) (return . Right . textToUtf8)) $ uniqueDefFields u}) $ constrUniques constr
        f autoKey cols uniqs refs' = (addTable', addInAlters') where
          (addInCreate, addInAlters') = addUniquesReferences uniqs refs'
          items = autoKey <> map showColumn cols <> addInCreate
          addTable' = "CREATE TABLE " <> (tableName escape e constr) <> " (" <> (intercalateS ", " items) <> ")"
        mkRefs = map (\r -> (Nothing, r))

      (migErrs, constrExisted, mig) = case tableStructure of
        Nothing -> let
          rest = AlterTable qualifiedCName addTable expectedTableStructure expectedTableStructure addInAlters
          in ([], False, [AddTable addTable, rest])
        Just oldTableStructure -> let
          alters = getAlters m oldTableStructure expectedTableStructure
          alterTable = if null alters
            then []
            else [AlterTable qualifiedCName (addTable) oldTableStructure expectedTableStructure alters]
          in ([], True, alterTable)
      -- this can happen when an ephemeral field was added. Consider doing something else except throwing an error
      allErrs = if constrExisted == triggerExisted || (constrExisted && null dels)
        then migErrs
        else ["Both trigger and constructor table must exist: " <> (T.pack $ show qualifiedCName)] <> migErrs
  return $ (constrExisted, if null allErrs
    then mergeMigrations $ map showAlterDb $ mig <> delTrigger <> updTriggers
    else Left allErrs)

-- on delete removes all ephemeral data
-- returns column name and delete statement for the referenced table
mkDeletes :: (Utf8 -> Utf8) -> (Utf8, DbType) -> [(Utf8, Utf8)]
mkDeletes esc = concat . traverseDbType f where
  f (DbList ref _) [col] = [(col, "DELETE FROM " <> esc (textToUtf8 ref) <> " WHERE id=old." <> esc (col) <>";")]
  f _ _ = []

showReferenceAction :: ReferenceActionType -> Utf8
showReferenceAction NoAction = "NO ACTION"
showReferenceAction Restrict = "RESTRICT"
showReferenceAction Cascade = "CASCADE"
showReferenceAction SetNull = "SET NULL"
showReferenceAction SetDefault = "SET DEFAULT"

readReferenceAction :: Text -> Maybe ReferenceActionType
readReferenceAction c = case c of
  "NO ACTION" -> Just NoAction
  "RESTRICT" -> Just Restrict
  "CASCADE" -> Just Cascade
  "SET NULL" -> Just SetNull
  "SET DEFAULT" -> Just SetDefault
  _ -> Nothing

class (Applicative m, Monad m) => SchemaAnalyzer m where
  schemaExists :: Utf8 -- ^ Schema name
               -> m Bool
  getCurrentSchema :: m (Maybe Utf8)
  listTables :: Maybe Text -- ^ Schema name
             -> m [Utf8]
  listTableTriggers :: QualifiedName -- ^ Qualified table name
                    -> m [Utf8]
  analyzeTable :: QualifiedName -- ^ Qualified table name
               -> m (Maybe TableInfo)
  analyzeTrigger :: QualifiedName -- ^ Qualified trigger name
                 -> m (Maybe Utf8)
  analyzeFunction :: QualifiedName -- ^ Qualified function name
                  -> m (Maybe (Maybe [DbTypePrimitive], Maybe DbTypePrimitive, Utf8)) -- ^ Argument types, return type, and body
  getMigrationPack :: m (MigrationPack m)

delim' :: Utf8
delim' = fromChar delimChar
