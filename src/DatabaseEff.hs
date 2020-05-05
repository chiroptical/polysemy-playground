{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module DatabaseEff where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Database
import           Database.Beam.Sqlite           ( runBeamSqlite
                                                , SqliteM
                                                )
import           Database.SQLite.Simple         ( open
                                                , close
                                                )

import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                , makeSem
                                                , Embed
                                                , interpret
                                                , embed
                                                )
import           Polysemy.Reader                ( Reader
                                                , ask
                                                )
import           Polysemy.Trace                 ( Trace
                                                , trace
                                                )
import           Polysemy.Error                 ( Error
                                                , throw
                                                , fromEither
                                                )

import           Control.Exception              ( bracket )

-- This function is defined in Polysemy.Error for 1.3.0.0
-- however w/ 1.3.0.0 this code won't compile on my machine
note :: Member (Error e) r => e -> Maybe a -> Sem r a
note e Nothing  = throw e
note _ (Just a) = pure a

data DatabaseEff m a where
  MakeTablesIfNotExists ::DatabaseEff m ()
  ListPersons ::Maybe Text -> Maybe Int -> Maybe Text -> DatabaseEff m [Person]
  CreatePerson ::PersonNoId -> DatabaseEff m Person
  ReadPerson ::Int -> DatabaseEff m Person
  UpdatePerson ::Person -> DatabaseEff m Person
  DestroyPerson ::Person -> DatabaseEff m ()

makeSem ''DatabaseEff

runQuery :: String -> SqliteM c -> IO c
runQuery s q = bracket (open s) close (`runBeamSqlite` q)

databaseEffToIO
  :: Members '[Embed IO, Reader String, Trace, Error DbErr] r
  => Sem (DatabaseEff ': r) a
  -> Sem r a
databaseEffToIO sem = do
  conn_s <- ask @String
  interpret
    (\case
      MakeTablesIfNotExists -> do
        trace "Stdout Log: makeTableIfNotExist called"
        embed $ bracket (open conn_s) close Database.makeTablesIfNotExists
      ListPersons mName mAge mAddr -> do
        trace "Stdout Log: listPersons called"
        embed . runQuery conn_s $ Database.listPersons mName mAge mAddr
      CreatePerson pNoId -> do
        trace "Stdout Log: insertPerson called"
        (embed . runQuery conn_s $ Database.createPerson pNoId) >>= fromEither
      ReadPerson id -> do
        trace "Stdout Log: readPerson called"
        (embed . runQuery conn_s $ Database.readPerson id)
          >>= note (PersonIdDoesNotExist id)
      UpdatePerson person -> do
        let Person {..} = person
        trace "Stdout Log: updatePerson called"
        (embed . runQuery conn_s $ Database.updatePerson person)
          >>= note (PersonDoesNotExist name)
      DestroyPerson person -> do
        trace "Stdout Log: destroyPerson called"
        embed . runQuery conn_s $ Database.destroyPersonByName person
    )
    sem
