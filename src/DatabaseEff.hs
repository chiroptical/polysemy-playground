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

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Trace
import           Polysemy.Error

import           Control.Exception              ( bracket )

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
        trace "CHIRO: makeTableIfNotExist called"
        embed $ bracket (open conn_s) close Database.makeTablesIfNotExists
      ListPersons mName mAge mAddr -> do
        trace "CHIRO: listPersons called"
        embed . runQuery conn_s $ Database.listPersons mName mAge mAddr
      CreatePerson pNoId -> do
        trace "CHIRO: insertPerson called"
        ePerson <- embed . runQuery conn_s $ Database.createPerson pNoId
        case ePerson of
          Right value -> return value
          Left err -> throw err
      ReadPerson id -> do
        trace "CHIRO: readPerson called"
        mPerson <- embed . runQuery conn_s $ Database.readPerson id
        case mPerson of
          Just person -> return person
          Nothing -> throw $ PersonIdDoesNotExist id
      UpdatePerson person -> do
        let Person {..} = person
        trace "CHIRO: updatePerson called"
        mPerson <- embed . runQuery conn_s $ Database.updatePerson person
        case mPerson of
          Just person -> return person
          Nothing -> throw $ PersonDoesNotExist name

      DestroyPerson person -> do
        trace "CHIRO: destroyPerson called"
        embed . runQuery conn_s $ Database.destroyPersonByName person
    )
    sem
