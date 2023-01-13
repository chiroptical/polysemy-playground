{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (
  MonadBeamInsertReturning,
  runInsertReturningList,
 )
import Database.Beam.Sqlite.Connection (Sqlite)
import Database.SQLite.Simple (Connection, execute_)
import GHC.Int (Int64)

data DbErr
  = PersonAlreadyExists Text
  | PersonIdDoesNotExist Int64
  | PersonDoesNotExist Text

data PersonNoId = PersonNoId
  { name :: Text
  , age :: Int64
  , address :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Person = Person
  { pKey :: Int64
  , name :: Text
  , age :: Int64
  , address :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

toPerson :: Person_ -> Person
toPerson Person_ {..} = Person _personId _personName _personAge _personAddress

data PersonT f = Person_
  { _personId :: C f Int64
  , _personName :: C f Text
  , _personAge :: C f Int64
  , _personAddress :: C f Text
  }
  deriving (Generic, Beamable)

type Person_ = PersonT Identity

deriving instance Show Person_

deriving instance Eq Person_

instance Table PersonT where
  data PrimaryKey PersonT f = PersonId (C f Int64)
    deriving (Generic, Beamable)
  primaryKey = PersonId . _personId

newtype PersonDb f = PersonDb
  { _personPerson :: f (TableEntity PersonT)
  }
  deriving (Generic, Database be)

personDb :: DatabaseSettings be PersonDb
personDb =
  defaultDbSettings
    `withDbModification` dbModification
      { _personPerson =
          setEntityName "person"
            <> modifyTableFields
              tableModification
                { _personId = "id"
                , _personName = "name"
                , _personAge = "age"
                , _personAddress = "address"
                }
      }

makeTablesIfNotExists :: Connection -> IO ()
makeTablesIfNotExists conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS person \
    \( id INTEGER PRIMARY KEY AUTOINCREMENT \
    \, name VARCHAR NOT NULL UNIQUE \
    \, age INTEGER NOT NULL \
    \, address VARCHAR NOT NULL \
    \)"

listPersons ::
  MonadBeam Sqlite m => Maybe Text -> Maybe Int64 -> Maybe Text -> m [Person]
listPersons mName mAge mAddr =
  (fmap . fmap) toPerson $
    runSelectReturningList $
      select $ do
        person_ <- all_ (_personPerson personDb)
        guard_ $ maybe (val_ True) ((_personName person_ ==.) . val_) mName
        guard_ $ maybe (val_ True) ((_personAge person_ ==.) . val_) mAge
        guard_ $ maybe (val_ True) ((_personAddress person_ ==.) . val_) mAddr
        return person_

createPerson ::
  (MonadFail m, MonadBeam Sqlite m, MonadBeamInsertReturning Sqlite m) =>
  PersonNoId ->
  m (Either DbErr Person)
createPerson PersonNoId {..} = do
  persons_ <- listPersons (Just name) Nothing Nothing
  if null persons_
    then do
      [person_] <-
        runInsertReturningList $
          insert (_personPerson personDb) $
            insertExpressions
              [Person_ default_ (val_ name) (val_ age) (val_ address)]
      return . Right . toPerson $ person_
    else return . Left $ PersonAlreadyExists name

readPersonByName :: MonadBeam Sqlite m => Text -> m (Maybe Person)
readPersonByName = (fmap . fmap) toPerson . readPersonByName_

readPersonByName_ :: MonadBeam Sqlite m => Text -> m (Maybe Person_)
readPersonByName_ name =
  runSelectReturningOne $
    select $ do
      person_ <- all_ (_personPerson personDb)
      guard_ (_personName person_ ==. val_ name)
      return person_

readPerson :: MonadBeam Sqlite m => Int64 -> m (Maybe Person)
readPerson = (fmap . fmap) toPerson . readPerson_

readPerson_ :: MonadBeam Sqlite m => Int64 -> m (Maybe Person_)
readPerson_ key =
  runSelectReturningOne $
    select $ do
      person_ <- all_ (_personPerson personDb)
      guard_ (_personId person_ ==. val_ key)
      return person_

updatePerson :: MonadBeam Sqlite m => Person -> m (Maybe Person)
updatePerson Person {..} = do
  mPerson_ <- readPerson_ pKey
  case mPerson_ of
    Nothing -> return Nothing
    Just person_ -> do
      let newPerson_ =
            person_
              { _personName = name
              , _personAge = age
              , _personAddress = address
              }
      runUpdate $ save (_personPerson personDb) newPerson_
      return . Just . toPerson $ newPerson_

destroyPersonByName :: MonadBeam Sqlite m => Person -> m ()
destroyPersonByName Person {..} =
  runDelete $
    delete
      (_personPerson personDb)
      ( \person_ ->
          _personName person_ ==. val_ name
            &&. _personAge person_ ==. val_ age
            &&. _personAddress person_
              ==. val_ address
      )
