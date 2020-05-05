module Main where

import           Api
import           DatabaseEff                    ( databaseEffToIO
                                                , makeTablesIfNotExists
                                                )

import           Database                       ( DbErr(..) )

import           Polysemy                       ( runM )
import           Polysemy.Reader                ( runReader )
import           Polysemy.Trace                 ( traceToIO )
import           Polysemy.Error                 ( runError )


import           Network.Wai.Handler.Warp      as Warp

main :: IO ()
main = do
  let dbName :: String
      dbName = "database.db"
  runM
    . runError @DbErr
    . traceToIO
    . runReader dbName
    . databaseEffToIO
    $ makeTablesIfNotExists
  app' <- app dbName
  Warp.run 8081 app'
