{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Polysemy (Members, Sem)
import Polysemy.Reader (Reader, ask)

newtype Config = Config
  { databaseConn :: String
  }
  deriving (Show)

program :: Members '[Reader String] r => Sem r Config
program = do
  cfg <- ask @String
  return $ Config cfg
