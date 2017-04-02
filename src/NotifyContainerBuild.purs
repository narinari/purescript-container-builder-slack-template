module NotifyContainerBuild where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Argonaut (decodeJson, encodeJson, jsonParser)
import Data.Either (Either(Left), either)

import Data.Traversable (for)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toMaybe)

import Data.HTTP.Method (Method(POST))

import Data.Google.CloudFunction.Event (FunctionEvent)
import Data.Google.CloudFunction.PubsubMessage (PubsubMessage)
import Data.Google.ContainerBuilder.Build (Build(..), BuildStatus(..), RepoSource(..), Revision(..), Source(..), StorageSource(..))
import Data.Slack.Message.Attachment (Attachment(..), Color(..), Field(..), PresetColor(..), defaultAttachment)

import Node.Buffer as Buf
import Node.Encoding (Encoding(Base64, UTF8))

import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)

import Debug.Trace

type FunctionEffects = (ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE, buffer :: Buf.BUFFER)

slackWebHookURL :: String
slackWebHookURL = "REPLACE ME"

notifyBuildPubSub :: FunctionEvent PubsubMessage -> Eff FunctionEffects Unit -> Eff FunctionEffects Unit
notifyBuildPubSub event callback = do
  let pubsubMessage = toMaybe event.data.data
  jsonString <- for pubsubMessage (flip Buf.fromString Base64 >=> Buf.toString UTF8)
  traceAnyA jsonString
  let build = maybe (Left "build resource nothing.")  (jsonParser >=> decodeJson) jsonString
  launchAff $ do
    res <- affjax $ defaultRequest
      { url = slackWebHookURL
      , method = Left POST
      , content = Just $ encodeJson $ either errorMessage buildMessage build
      }
    liftEff $ log res.response
  callback

  where
  errorMessage err
    = Attachment defaultAttachment
      { fallback = "Container build result parse error!:skull:"
      , title = "Container build result parse error!:skull:"
      , text =  err
      , color = Just $ Preset Danger
      }

  buildMessage (Build build)
    = Attachment $ defaultAttachment
      { fallback = message build.status
      , title = "Container build"
      , color = Just $ case build.status of
          StatusUnknown -> Preset Danger
          Queued -> CustomColor "#439FE0"
          Working -> CustomColor "#439FE0"
          Success -> Preset Good
          Failure -> Preset Danger
          InternalError -> Preset Danger
          Timeout -> Preset Danger
          Cancelled -> Preset Warning
      , text = message build.status
      , fields =
        [ sourceField build
        , revisionField build
        , buildIdField build
        ]
      }

  message status = "Container " <> case status of
    StatusUnknown -> "build unknown.:grey_question:"
    Queued -> "build queued.:parking:"
    Working -> "building.:bicyclist::dash:"
    Success -> "build successful!:+1:"
    Failure -> "build error!:scream:"
    InternalError -> "build internal error!:fearful:"
    Timeout -> "build timeout.:construction:"
    Cancelled -> "build canceled.:no_bicycles:"

  sourceField build = Field
    { title: case build.source of
        Storage _ -> "Storage"
        Repo _ -> "Repository"
    , value: case build.source of
        Storage (StorageSource storage) -> storage.bucket <> "/" <> storage.object
        Repo (RepoSource repo) -> repo.repoName
    , short: true
    }

  revisionField build = Field
    { title: case build.source of
        Storage (StorageSource storage) -> "Generation"
        Repo (RepoSource repo) -> case repo.revision of
          BranchName _ -> "Branch"
          TagName _ -> "Tag"
          CommitSha _ ->"Commit"
    , value: case build.source of
        Storage (StorageSource storage) -> storage.generation
        Repo (RepoSource repo) -> case repo.revision of
          BranchName name -> name
          TagName name -> name
          CommitSha sha -> sha
    , short: true
    }

  buildIdField build = Field
    { title: "Build ID"
    , value: "<" <> build.logUrl <> "|" <> build.id_ <> ">"
    , short: false
    }