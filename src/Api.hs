{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Api where

import           Database                       ( Person(..)
                                                , PersonNoId(..)
                                                , DbErr(..)
                                                )
import           DatabaseEff
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Trace
import           Polysemy.Error

import           Control.Monad.Trans.Except

import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as L
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )

type QPStrict = QueryParam' '[Strict, Required]

-- GET /person?name=...&age=...&address=... is Read w/ optional query parameters
-- GET /person/{id} is Read w/ specific ID
-- POST /person is Create
-- PUT /person is Update
-- DELETE /person is Destroy
data Routes route = Routes
  { _get :: route :- "person" :> QueryParam "name" Text :> QueryParam "age" Int :> QueryParam "address" Text :> Get '[ JSON] [Person]
  , _getById :: route :- "person" :> Capture "id" Int :> Get '[ JSON] Person
  , _post :: route :- "person" :> ReqBody '[ JSON] PersonNoId :> Post '[ JSON] Person
  , _put :: route :- "person" :> ReqBody '[ JSON] Person :> Put '[ JSON] Person
  , _delete :: route :- "person" :> ReqBody '[ JSON] Person :> Delete '[ JSON] ()
  } deriving Generic

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

record :: Member DatabaseEff r => Routes (AsServerT (Sem r))
record = Routes { _get     = listPersons
                , _getById = readPerson
                , _post    = createPerson
                , _put     = updatePerson
                , _delete  = destroyPerson
                }

app :: String -> IO Application
app conn = return $ genericServeT (interpretServer conn) record
 where
  interpretServer conn sem =
    liftToHandler
      . runM
      . runError @DbErr
      . traceToIO
      . runReader conn
      . databaseEffToIO
      $ sem
  liftToHandler = Handler . ExceptT . fmap handleErrors

handleErrors :: Either DbErr a -> Either ServerError a
handleErrors (Left (PersonAlreadyExists name)) = Left err409
  { errBody = mconcat
                [ "Person with name `"
                , encodeUtf8 . L.fromStrict $ name
                , "` already exists..."
                ]
  }
handleErrors (Left (PersonIdDoesNotExist id)) = Left err404
  { errBody = mconcat
                [ "Person with id `"
                , encodeUtf8 . L.pack $ show id
                , "` does not exists..."
                ]
  }
handleErrors (Left (PersonDoesNotExist name)) = Left err404
  { errBody = mconcat
                [ "Person with name `"
                , encodeUtf8 . L.fromStrict $ name
                , "` does not exists..."
                ]
  }
handleErrors (Right value) = Right value
