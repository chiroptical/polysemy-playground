{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module DatabaseEff where

import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database
import           Database.Beam.Sqlite   (runBeamSqlite)
import           Database.SQLite.Simple (open)
import           Polysemy
import           Polysemy.Reader
import           Polysemy.Trace

data DatabaseEff m a where
  MakeTablesIfNotExists :: DatabaseEff m ()
  ListPersons :: Maybe Text -> Maybe Int -> Maybe Text -> DatabaseEff m [Person]
  CreatePerson :: PersonNoId -> DatabaseEff m Person
  ReadPerson :: Int -> DatabaseEff m (Maybe Person)
  UpdatePerson :: Person -> DatabaseEff m (Maybe Person)
  DestroyPerson :: Person -> DatabaseEff m ()

makeSem ''DatabaseEff

-- Problem: We would like ReadPerson & UpdatePerson to be (... -> Person)
--  however it is unclear how to throw an error using Polysemy.
--  If we can figure out how to throw, we can return the relevant HTTP codes
databaseEffToIO ::
     Members '[ Embed IO, Reader String, Trace] r
  => Sem (DatabaseEff ': r) a
  -> Sem r a
databaseEffToIO sem = do
  conn_s <- ask @String
  interpret
    (\case
       MakeTablesIfNotExists -> do
         trace "CHIRO: makeTableIfNotExist called"
         embed $ do
           conn <- open conn_s
           Database.makeTablesIfNotExists conn
       ListPersons mName mAge mAddr -> do
         trace "CHIRO: listPersons called"
         embed $ do
           conn <- open conn_s
           runBeamSqlite conn $ Database.listPersons mName mAge mAddr
       CreatePerson pNoId -> do
         trace "CHIRO: insertPerson called"
         embed $ do
           conn <- open conn_s
           runBeamSqlite conn $ Database.createPerson pNoId
       ReadPerson id -> do
         trace "CHIRO: readPerson called"
         embed $ do
           conn <- open conn_s
           runBeamSqlite conn $ Database.readPerson id
       UpdatePerson person ->
         embed $ do
           conn <- open conn_s
           runBeamSqlite conn $ Database.updatePerson person
       DestroyPerson person ->
         embed $ do
           conn <- open conn_s
           runBeamSqlite conn $ Database.destroyPersonByName person)
    sem
