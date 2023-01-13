{-# LANGUAGE TypeApplications #-}

module Main where

import Api
import DatabaseEff (
  databaseEffToIO,
  makeTablesIfNotExists,
 )

import Database (DbErr (..))

import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.Reader (runReader)

import Colog.Polysemy (runLogAction)
import Message (logMessageStdout)

import Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  let dbName :: String
      dbName = "database.db"
  _ <-
    runM
      . runError @DbErr
      . runLogAction @IO logMessageStdout
      . runReader dbName
      . databaseEffToIO
      $ makeTablesIfNotExists
  app' <- app dbName
  Warp.run 8081 app'
