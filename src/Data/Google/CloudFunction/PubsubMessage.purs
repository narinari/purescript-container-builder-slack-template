module Data.Google.CloudFunction.PubsubMessage where

import Data.Nullable (Nullable)
import Data.StrMap as M

type PubsubMessage =
  { data :: Nullable String
  , attribute :: M.StrMap String
  , messageId :: String
  , publishTime :: String
  }
