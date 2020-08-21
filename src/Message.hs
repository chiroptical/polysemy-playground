{-# LANGUAGE RecordWildCards #-}

module Message where

import           Colog.Core             (LogAction (LogAction), Severity (..))
import           Control.Monad.IO.Class (MonadIO (liftIO))

data Message =
  Message
    { severity :: Severity
    , message  :: String
    }
  deriving (Eq)

logMessageStdout :: LogAction IO Message
logMessageStdout = LogAction $ liftIO . putStrMessage
  where
    putStrMessage :: Message -> IO ()
    putStrMessage Message {..}
      | severity == Debug = putStrLn $ "[DEBUG]: " ++ message
      | severity == Info = putStrLn $ "[INFO]: " ++ message
      | severity == Warning = putStrLn $ "[WARNING]: " ++ message
      | severity == Error = putStrLn $ "[ERROR]: " ++ message
