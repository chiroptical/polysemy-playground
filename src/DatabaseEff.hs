{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module DatabaseEff where

import           Colog.Core             (Severity (..))
import           Colog.Polysemy         (Log, log)
import           Control.Exception      (bracket)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database
import           Database.Beam.Sqlite   (SqliteM, runBeamSqlite)
import           Database.SQLite.Simple (close, open)
import           Message                (Message (..))
import           Polysemy               (Embed, Member, Members, Sem, embed,
                                         interpret, makeSem)
import           Polysemy.Error         (Error, fromEither, throw)
import           Polysemy.Reader        (Reader, ask)
import           Polysemy.Trace         (Trace, trace)
import           Prelude                hiding (log)

-- This function is defined in Polysemy.Error for 1.3.0.0
-- however w/ 1.3.0.0 this code won't compile on my machine
note :: Member (Error e) r => e -> Maybe a -> Sem r a
note e Nothing  = throw e
note _ (Just a) = pure a

data DatabaseEff m a where
  MakeTablesIfNotExists :: DatabaseEff m ()
  ListPersons :: Maybe Text -> Maybe Int -> Maybe Text -> DatabaseEff m [Person]
  CreatePerson :: PersonNoId -> DatabaseEff m Person
  ReadPerson :: Int -> DatabaseEff m Person
  UpdatePerson :: Person -> DatabaseEff m Person
  DestroyPerson :: Person -> DatabaseEff m ()

makeSem ''DatabaseEff

runQuery :: String -> SqliteM c -> IO c
runQuery s q = bracket (open s) close (`runBeamSqlite` q)

databaseEffToIO ::
     Members '[ Embed IO, Reader String, Log Message, Error DbErr] r
  => Sem (DatabaseEff ': r) a
  -> Sem r a
databaseEffToIO sem = do
  conn_s <- ask @String
  interpret
    (\case
       MakeTablesIfNotExists -> do
         log $ Message Info "makeTableIfNotExist called"
         embed $ bracket (open conn_s) close Database.makeTablesIfNotExists
       ListPersons mName mAge mAddr -> do
         log $ Message Info "listPersons called"
         embed . runQuery conn_s $ Database.listPersons mName mAge mAddr
       CreatePerson pNoId -> do
         log $ Message Info "insertPerson called"
         (embed . runQuery conn_s $ Database.createPerson pNoId) >>= fromEither
       ReadPerson id -> do
         log $ Message Info "readPerson called"
         (embed . runQuery conn_s $ Database.readPerson id) >>=
           note (PersonIdDoesNotExist id)
       UpdatePerson person -> do
         let Person {..} = person
         log $ Message Info "updatePerson called"
         (embed . runQuery conn_s $ Database.updatePerson person) >>=
           note (PersonDoesNotExist name)
       DestroyPerson person -> do
         log $ Message Info "destroyPerson called"
         embed . runQuery conn_s $ Database.destroyPersonByName person)
    sem
