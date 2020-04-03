module Api where

import           Database                   (Person (..), PersonNoId (..))
import           DatabaseEff
import           Servant

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Trace

import           Control.Monad.Trans.Except

import           Data.Text                  (Text)

type QPStrict = QueryParam' '[ Strict, Required]

-- GET /person?name=...&age=...&address=... is Read w/ optional query parameters
-- GET /person/{id} is Read w/ specific ID
-- POST /person is Create
-- PUT /person is Update
-- DELETE /person is Destroy
type Api
   =    "person" :> QueryParam "name" Text :> QueryParam "age" Int :> QueryParam "address" Text :> Get '[ JSON] [Person]
   :<|> "person" :> Capture "id" Int :> Get '[ JSON] (Maybe Person)
   :<|> "person" :> ReqBody '[ JSON] PersonNoId :> Post '[ JSON] Person
   :<|> "person" :> ReqBody '[ JSON] Person :> Put '[ JSON] (Maybe Person)
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
      liftToHandler . runM . traceToIO . runReader conn . databaseEffToIO $ sem
    liftToHandler :: IO a -> Handler a
    liftToHandler = Handler . ExceptT . fmap pure
