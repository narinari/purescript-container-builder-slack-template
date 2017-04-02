module Data.Google.ContainerBuilder.Build where

import Prelude

import Control.Alt ((<|>))

import Data.Argonaut (class DecodeJson, foldJsonString, decodeJson)
import Data.Argonaut.Decode.Combinators ((.??), (.?))
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.StrMap as M

data BuildStatus
  = StatusUnknown
  | Queued
  | Working
  | Success
  | Failure
  | InternalError
  | Timeout
  | Cancelled

instance decodeJsonBuildStatus :: DecodeJson BuildStatus where
  decodeJson = foldJsonString (Left "unexpected type") case _ of
    "STATUS_UNKNOWN" -> Right StatusUnknown
    "QUEUED" -> Right Queued
    "WORKING" -> Right Working
    "SUCCESS" -> Right Success
    "FAILURE" -> Right Failure
    "INTERNAL_ERROR" -> Right InternalError
    "TIMEOUT" -> Right Timeout
    "CANCELLED" -> Right Cancelled
    _ -> Left "unknown status"

data StorageSource = StorageSource
  { bucket :: String
  , object :: String
  , generation :: String
  }

derive instance genericStorageSource :: Generic StorageSource
instance decodeJsonStorageSource :: DecodeJson StorageSource where
  decodeJson = decodeJson >=> \obj ->
    { bucket: _
    , object: _
    , generation: _
    }
    <$> (obj .? "bucket")
    <*> (obj .? "object")
    <*> (obj .? "generation")
    <#> StorageSource

data RepoSource = RepoSource
  { projectId :: String
  , repoName :: String
  , revision :: Revision
  }

derive instance genericRepoSource :: Generic RepoSource
instance decodeJsonRepoSource :: DecodeJson RepoSource where
  decodeJson = decodeJson >=> \obj ->
    { projectId: _
    , repoName: _
    , revision: _
    }
    <$> (obj .? "projectId")
    <*> (obj .? "repoName")
    <*> ((BranchName <$> obj .? "branchName") <|> (TagName <$> obj .? "tagName") <|> (CommitSha <$> obj .? "commitSha"))
    <#> RepoSource

data Source
  = Storage StorageSource
  | Repo RepoSource

derive instance genericSource :: Generic Source
instance decodeJsonSource :: DecodeJson Source where
  decodeJson = decodeJson >=> \obj ->
    (Storage <$> obj .? "storageSource") <|> (Repo <$> obj .? "repoSource")

data Revision
  = BranchName String
  | TagName String
  | CommitSha String

derive instance genericRevision :: Generic Revision

data BuildStep = BuildStep
  { name :: String
  , env :: Array String
  , args :: Array String
  , dir :: Maybe String
  , id_ :: Maybe String
  , waitFor :: Array String
  , entrypoint :: Maybe String
  }

derive instance genericBuildStep :: Generic BuildStep
instance decodeJsonBuildStep :: DecodeJson BuildStep where
  decodeJson = decodeJson >=> \obj ->
    { name: _
    , env: _
    , args: _
    , dir: _
    , id_: _
    , waitFor: _
    , entrypoint: _
    }
    <$> (obj .? "name")
    <*> (obj .?? "env" <#> fromMaybe [])
    <*> (obj .? "args")
    <*> (obj .?? "dir")
    <*> (obj .?? "id")
    <*> (obj .?? "waitFor" <#> fromMaybe [])
    <*> (obj .?? "entrypoint")
    <#> BuildStep

data Results = Results
  { images :: Array BuildImage
  , buildStepImages :: Array String
  }

derive instance genericResults :: Generic Results
instance decodeJsonResults :: DecodeJson Results where
  decodeJson = decodeJson >=> \obj ->
    { images: _
    , buildStepImages: _
    }
    <$> (obj .? "images")
    <*> (obj .? "buildStepImages")
    <#> Results

data BuildImage = BuildImage
  { name :: String
  , digest :: String
  }

derive instance genericBuildImage :: Generic BuildImage
instance decodeJsonBuildImage :: DecodeJson BuildImage where
  decodeJson = decodeJson >=> \obj ->
    { name: _
    , digest: _
    }
    <$> (obj .? "name")
    <*> (obj .? "digest")
    <#> BuildImage

data FileHashes = FileHashes { fileHash :: Array Hash }

derive instance genericFileHashes :: Generic FileHashes
instance decodeJsonFileHashes :: DecodeJson FileHashes where
  decodeJson = decodeJson >=> \obj ->
    { fileHash: _
    }
    <$> (obj .? "fileHash")
    <#> FileHashes

data SourceProvenance = SourceProvenance
  { resolvedStorageSource :: Maybe StorageSource
  , resolvedRepoSource :: Maybe RepoSource
  , fileHashes :: M.StrMap FileHashes
  }

instance decodeJsonSourceProvenance :: DecodeJson SourceProvenance where
  decodeJson = decodeJson >=> \obj ->
    { resolvedStorageSource: _
    , resolvedRepoSource: _
    , fileHashes: _
    }
    <$> (obj .?? "resolvedStorageSource")
    <*> (obj .?? "resolvedRepoSource")
    <*> (obj .?? "fileHashes" <#> fromMaybe M.empty)
    <#> SourceProvenance

data Hash = Hash
  { type :: HashType
  , value :: String
  }

derive instance genericHash :: Generic Hash
instance decodeJsonHash :: DecodeJson Hash where
  decodeJson = decodeJson >=> \obj ->
    { type: _
    , value: _
    }
    <$> (obj .? "type")
    <*> (obj .? "value")
    <#> Hash

data HashType
  = None
  | SHA256

derive instance genericHashType :: Generic HashType
instance decodeJsonHashType :: DecodeJson HashType where
  decodeJson = foldJsonString (Left "unexpected type") case _ of
    "NONE" -> Right None
    "SHA256" -> Right SHA256
    _ -> Left "unknown status"

data BuildOptions = BuildOptions
  { sourceProvenanceHash :: HashType
  , requestedVerifyOption :: VerifyOption
  }

derive instance genericBuildOptions :: Generic BuildOptions
instance decodeJsonBuildOptions :: DecodeJson BuildOptions where
  decodeJson = decodeJson >=> \obj ->
    { sourceProvenanceHash: _
    , requestedVerifyOption: _
    }
    <$> (obj .? "sourceProvenanceHash")
    <*> (obj .? "requestedVerifyOption")
    <#> BuildOptions

data VerifyOption
  = NotVerified
  | Verified

derive instance genericVerifyOption :: Generic VerifyOption
instance decodeJsonVerifyOption :: DecodeJson VerifyOption where
  decodeJson = foldJsonString (Left "unexpected type") case _ of
    "NOT_VERIFIED" -> Right NotVerified
    "VERIFIED" -> Right Verified
    _ -> Left "unknown status"

data Build = Build
  { id_ :: String
  , projectId :: String
  , status :: BuildStatus
  , statusDetail :: Maybe String
  , source :: Source
  , steps :: Array BuildStep
  , results :: Maybe Results
  , createTime :: String
  , startTime :: Maybe String
  , finishTime :: Maybe String
  , timeout :: String
  , images :: Array String
  , logsBucket :: Maybe String
  , sourceProvenance :: SourceProvenance
  , buildTriggerId :: Maybe String
  , options :: Maybe BuildOptions
  , logUrl :: String
  , substitutions :: M.StrMap String
  }

instance decodeJsonBuild :: DecodeJson Build where
  decodeJson = decodeJson >=> \obj ->
    { id_: _
    , projectId: _
    , status: _
    , statusDetail: _
    , source: _
    , steps: _
    , results: _
    , createTime: _
    , startTime: _
    , finishTime: _
    , timeout: _
    , images: _
    , logsBucket: _
    , sourceProvenance: _
    , buildTriggerId: _
    , options: _
    , logUrl: _
    , substitutions: _
    }
    <$> (obj .? "id")
    <*> (obj .? "projectId")
    <*> (obj .? "status")
    <*> (obj .?? "statusDetail")
    <*> (obj .? "source")
    <*> (obj .? "steps")
    <*> (obj .?? "results")
    <*> (obj .? "createTime")
    <*> (obj .?? "startTime")
    <*> (obj .?? "finishTime")
    <*> (obj .? "timeout")
    <*> (obj .? "images")
    <*> (obj .?? "logsBucket")
    <*> (obj .? "sourceProvenance")
    <*> (obj .?? "buildTriggerId")
    <*> (obj .?? "options")
    <*> (obj .? "logUrl")
    <*> (obj .?? "substitutions" <#> fromMaybe M.empty)
    <#> Build
