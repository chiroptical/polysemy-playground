{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Hspec (
  Spec,
  around,
  describe,
  hspec,
  it,
  pending,
  runIO,
  shouldBe,
 )

import Database (
  DbErr (..),
  Person (..),
  PersonNoId (..),
 )
import DatabaseEff (
  DatabaseEff,
  databaseEffToIO,
  makeTablesIfNotExists,
 )

import Polysemy (
  Embed,
  Member,
  Members,
  Sem,
  embed,
  runM,
 )
import Polysemy.Error (Error, fromEither, runError)
import Polysemy.Reader (runReader)
import Polysemy.Trace (traceToIO)

import Api (
  Routes (..),
  api,
  app,
  handleErrors,
  record,
 )

import Data.Proxy (Proxy (..))

import Servant.API.Generic (ToServantApi, (:-))
import Servant.Client
import Servant.Client.Generic (AsClientT, genericClientHoist)

import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp

import Colog.Polysemy (runLogAction)
import Data.Text (Text)
import Message (logMessageStdout)
import System.Directory (removeFile)

withPersonApi :: (Warp.Port -> IO ()) -> IO ()
withPersonApi action = do
  let dbName :: String
      dbName = "test.db"
  runM
    . runError @DbErr
    . runLogAction @IO logMessageStdout
    . runReader dbName
    . databaseEffToIO
    $ makeTablesIfNotExists
  app' <- app dbName
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  Warp.testWithApplication (pure app') action

cliRoutes ::
  Members '[Embed IO, Error ClientError] r =>
  ClientEnv ->
  Routes (AsClientT (Sem r))
cliRoutes cenv = genericClientHoist nt
  where
    nt :: Members '[Embed IO, Error ClientError] r => ClientM a -> Sem r a
    nt cm = (embed $ runClientM cm cenv) >>= fromEither

cliGet ::
  Members '[Embed IO, Error ClientError] r =>
  ClientEnv ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Sem r [Person]
cliGet cenv = _get (cliRoutes cenv)

cliGetById ::
  Members '[Embed IO, Error ClientError] r =>
  ClientEnv ->
  Int ->
  Sem r Person
cliGetById cenv = _getById (cliRoutes cenv)

cliPost ::
  Members '[Embed IO, Error ClientError] r =>
  ClientEnv ->
  PersonNoId ->
  Sem r Person
cliPost cenv = _post (cliRoutes cenv)

businessLogicSpec :: Spec
businessLogicSpec =
  around withPersonApi $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})
    describe "POST followed by GET" $ do
      let personId = 1
      let examplePerson = PersonNoId "chiroptical" 31 "123 example st."
      let examplePersonWithId =
            Person personId "chiroptical" 31 "123 example st."
      it "POST /person with fixture" $ \port -> do
        result <-
          runM @IO . runError @ClientError $
            cliPost (clientEnv port) examplePerson
        result `shouldBe` (Right examplePersonWithId)
      it "GET /person/1" $ \port -> do
        result <-
          runM @IO . runError @ClientError $
            cliGetById (clientEnv port) personId
        result `shouldBe` (Right examplePersonWithId)
      it "GET /person" $ \port -> do
        result <-
          runM @IO . runError @ClientError $
            cliGet (clientEnv port) Nothing Nothing Nothing
        result `shouldBe` (Right [examplePersonWithId])

spec :: Spec
spec = businessLogicSpec

main :: IO ()
main = hspec spec
