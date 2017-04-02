module Data.Google.CloudFunction.Event where

type FunctionEvent a =
  { eventId	:: String
  , timestamp	:: String
  , eventType	:: String
  , resource :: String
  , data :: a
  }