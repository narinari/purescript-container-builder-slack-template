module Data.Google.ContainerBuilder.Build where

import Prelude

import Control.Alt ((<|>))

import Data.Argonaut (class DecodeJson, foldJsonString, decodeJson)
import Data.Argonaut.Decode.Combinators ((.??), (.?))
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.StrMap as M

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/projects.builds#Build.Status
data BuildStatus
  = StatusUnknown
  | Queued
  | Working
  | Success
  | Failure
  | InternalError
  | Timeout
  | Cancelled

instance showBuildStatus :: Show BuildStatus where
  show = case _ of
    StatusUnknown -> "Status Unknown"
    Queued -> "Queued"
    Working -> "Working"
    Success -> "Success"
    Failure -> "Failure"
    InternalError -> "Internal　Error"
    Timeout -> "Timeout"
    Cancelled -> "Cancelled"

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

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/projects.builds#Build.StorageSource
data StorageSource = StorageSource
  { bucket :: String
  , object :: String
  , generation :: String
  }

derive instance eqStorageSource :: Eq StorageSource

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

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/RepoSource
data RepoSource = RepoSource
  { projectId :: String
  , repoName :: String
  , revision :: Revision
  }

derive instance eqRepoSource :: Eq RepoSource

data Revision
  = BranchName String
  | TagName String
  | CommitSha String

derive instance eqRevision :: Eq Revision

instance decodeJsonRepoSource :: DecodeJson RepoSource where
  decodeJson = decodeJson >=> \obj ->
    { projectId: _
    , repoName: _
    , revision: _
    }
    <$> (obj .? "projectId")
    <*> (obj .? "repoName")
    <*> (BranchName <$> obj .? "branchName" <|>
         TagName <$> obj .? "tagName" <|>
         CommitSha <$> obj .? "commitSha")
    <#> RepoSource

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/projects.builds#Build.Source
data Source
  = Storage StorageSource
  | Repo RepoSource

derive instance eqSource :: Eq Source

instance decodeJsonSource :: DecodeJson Source where
  decodeJson = decodeJson >=> \obj ->
    (Storage <$> obj .? "storageSource") <|> (Repo <$> obj .? "repoSource")

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/projects.builds#Build.BuildStep
data BuildStep = BuildStep
  { name :: String
  , env :: Array String
  , args :: Array String
  , dir :: Maybe String
  , id_ :: Maybe String
  , waitFor :: Array String
  , entrypoint :: Maybe String
  }

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

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/projects.builds#Build.Results
data Results = Results
  { images :: Array BuildImage
  , buildStepImages :: Array String
  }

instance decodeJsonResults :: DecodeJson Results where
  decodeJson = decodeJson >=> \obj ->
    { images: _
    , buildStepImages: _
    }
    <$> (obj .? "images")
    <*> (obj .? "buildStepImages")
    <#> Results

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/projects.builds#Build.BuiltImage
data BuildImage = BuildImage
  { name :: String
  , digest :: String
  }

instance decodeJsonBuildImage :: DecodeJson BuildImage where
  decodeJson = decodeJson >=> \obj ->
    { name: _
    , digest: _
    }
    <$> (obj .? "name")
    <*> (obj .? "digest")
    <#> BuildImage

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/projects.builds#Build.SourceProvenance
data SourceProvenance = SourceProvenance
  { resolvedSource :: Source
  , fileHashes :: M.StrMap FileHashes
  }

instance decodeJsonSourceProvenance :: DecodeJson SourceProvenance where
  decodeJson = decodeJson >=> \obj ->
    { resolvedSource: _
    , fileHashes: _
    }
    <$> (Storage <$> obj .? "resolvedStorageSource" <|>
         Repo <$> obj .? "resolvedRepoSource"
        )
    <*> (obj .?? "fileHashes" <#> fromMaybe M.empty)
    <#> SourceProvenance

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/FileHashes
data FileHashes = FileHashes { fileHash :: Array Hash }

instance decodeJsonFileHashes :: DecodeJson FileHashes where
  decodeJson = decodeJson >=> \obj ->
    { fileHash: _
    }
    <$> (obj .? "fileHash")
    <#> FileHashes

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/FileHashes#Hash
data Hash = Hash
  { type :: HashType
  , value :: String
  }

instance decodeJsonHash :: DecodeJson Hash where
  decodeJson = decodeJson >=> \obj ->
    { type: _
    , value: _
    }
    <$> (obj .? "type")
    <*> (obj .? "value")
    <#> Hash

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/projects.builds#Build.HashType
data HashType
  = None
  | SHA256

instance decodeJsonHashType :: DecodeJson HashType where
  decodeJson = foldJsonString (Left "unexpected type") case _ of
    "NONE" -> Right None
    "SHA256" -> Right SHA256
    _ -> Left "unknown status"

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/projects.builds#Build.BuildOptions
data BuildOptions = BuildOptions
  { sourceProvenanceHash :: HashType
  , requestedVerifyOption :: VerifyOption
  }

instance decodeJsonBuildOptions :: DecodeJson BuildOptions where
  decodeJson = decodeJson >=> \obj ->
    { sourceProvenanceHash: _
    , requestedVerifyOption: _
    }
    <$> (obj .? "sourceProvenanceHash")
    <*> (obj .? "requestedVerifyOption")
    <#> BuildOptions

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/projects.builds#Build.VerifyOption
data VerifyOption
  = NotVerified
  | Verified

instance decodeJsonVerifyOption :: DecodeJson VerifyOption where
  decodeJson = foldJsonString (Left "unexpected type") case _ of
    "NOT_VERIFIED" -> Right NotVerified
    "VERIFIED" -> Right Verified
    _ -> Left "unknown status"

-- | https://cloud.google.com/container-builder/docs/api/reference/rest/v1/projects.builds#Build
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
