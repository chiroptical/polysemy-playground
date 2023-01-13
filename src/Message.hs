{-# LANGUAGE RecordWildCards #-}

module Message where

import Colog.Core (LogAction (LogAction), Severity (..))
import Control.Monad.IO.Class (MonadIO (liftIO))

data Message = Message
  { severity :: Severity
  , message :: String
  }
  deriving (Eq)

logMessageStdout :: LogAction IO Message
logMessageStdout = LogAction $ liftIO . putStrMessage
  where
    putStrMessage :: Message -> IO ()
    putStrMessage Message {..} =
      case severity of
        Debug -> putStrLn $ "[DEBUG]: " ++ message
        Info -> putStrLn $ "[INFO]: " ++ message
        Warning -> putStrLn $ "[WARNING]: " ++ message
        Error -> putStrLn $ "[ERROR]: " ++ message
