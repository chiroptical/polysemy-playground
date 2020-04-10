{-# LANGUAGE TypeApplications #-}

module Lib where

import           Polysemy
import           Polysemy.Reader

-- This was used as an example to understand very basic Polysemy
newtype Config =
  Config
    { databaseConn :: String
    }
  deriving (Show)

program :: Members '[Reader String] r => Sem r Config
program = do
  cfg <- ask @String
  return $ Config cfg
