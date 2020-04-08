module Api where

import           Database                   (Person (..), PersonNoId (..), DbErr (..))
import           DatabaseEff
import           Servant

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Trace
import           Polysemy.Error

import           Control.Monad.Trans.Except

import           Data.Text                  (Text)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (encodeUtf8)

type QPStrict = QueryParam' '[ Strict, Required]

-- GET /person?name=...&age=...&address=... is Read w/ optional query parameters
-- GET /person/{id} is Read w/ specific ID
-- POST /person is Create
-- PUT /person is Update
-- DELETE /person is Destroy
type Api
   =    "person" :> QueryParam "name" Text :> QueryParam "age" Int :> QueryParam "address" Text :> Get '[ JSON] [Person]
   :<|> "person" :> Capture "id" Int :> Get '[ JSON] Person
   :<|> "person" :> ReqBody '[ JSON] PersonNoId :> Post '[ JSON] Person
   :<|> "person" :> ReqBody '[ JSON] Person :> Put '[ JSON] Person
   :<|> "person" :> ReqBody '[ JSON] Person :> Delete '[ JSON] ()

server :: Member DatabaseEff r => ServerT Api (Sem r)
server =
  listPersons :<|> readPerson :<|> createPerson :<|> updatePerson :<|>
  destroyPerson

personApi :: Proxy Api
personApi = Proxy

createApp :: String -> IO Application
createApp conn =
  return
    (serve personApi $ hoistServer personApi (`interpretServer` conn) server)
  where
    interpretServer sem conn =
      liftToHandler . runM . runError @DbErr . traceToIO . runReader conn . databaseEffToIO $ sem
    liftToHandler = Handler . ExceptT . fmap handleErrors
    handleErrors (Left (PersonAlreadyExists name)) = Left err409 { errBody = mconcat ["Person with name `", encodeUtf8 . L.fromStrict $ name, "` already exists..."] }
    handleErrors (Left (PersonIdDoesNotExist id)) = Left err404 { errBody = mconcat ["Person with id `", encodeUtf8 . L.pack $ show id, "` does not exists..."] }
    handleErrors (Left (PersonDoesNotExist name)) = Left err404 { errBody = mconcat ["Person with name `", encodeUtf8 . L.fromStrict $ name, "` does not exists..."] }
    handleErrors (Right value) = Right value
