module Main where

import           Api
import           DatabaseEff              (databaseEffToIO,
                                           makeTablesIfNotExists)

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Trace

import           Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  let dbName :: String
      dbName = "database.db"
  runM . traceToIO . runReader dbName . databaseEffToIO $ makeTablesIfNotExists
  app <- createApp dbName
  Warp.run 8081 app
