module Data.Slack.Message.Attachment where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, jsonEmptyObject, (:=), (~>))
import Data.Maybe (Maybe(..))

data PresetColor
  = Good
  | Warning
  | Danger

instance showPresetColor :: Show PresetColor where
  show = case _ of
    Good -> "good"
    Warning -> "warning"
    Danger -> "danger"

instance encodeJsonPresetColor :: EncodeJson PresetColor where
  encodeJson = show >>> encodeJson

data Color
  = Preset PresetColor
  | CustomColor String

instance showColor :: Show Color where
  show (Preset color) = show color
  show (CustomColor s) = s

instance encodeJsonColor :: EncodeJson Color where
  encodeJson = show >>> encodeJson

data Field = Field
  { title :: String
  , value :: String
  , short :: Boolean
  }

instance encodeJsonField :: EncodeJson Field where
  encodeJson (Field r)
    =  "title" := r.title
    ~> "value" := r.value
    ~> "short" := r.short
    ~> jsonEmptyObject

type AttachmentRecord =
  { fallback :: String
  , color :: Maybe Color
  , pretext :: Maybe String
  , authorName :: Maybe String
  , authorLink :: Maybe String
  , authorIcon :: Maybe String
  , title :: String
  , titleLink :: Maybe String
  , text :: String
  , fields :: Array Field
  , imageUrl :: Maybe String
  , thumbUrl :: Maybe String
  , footer :: Maybe String
  , footerIcon :: Maybe String
  , ts :: Maybe Int
  }

defaultAttachment :: AttachmentRecord
defaultAttachment =
  { fallback: ""
  , color: Nothing
  , pretext: Nothing
  , authorName: Nothing
  , authorLink: Nothing
  , authorIcon: Nothing
  , title: ""
  , titleLink: Nothing
  , text: ""
  , fields: []
  , imageUrl: Nothing
  , thumbUrl: Nothing
  , footer: Nothing
  , footerIcon: Nothing
  , ts: Nothing
  }

data Attachment = Attachment AttachmentRecord

instance encodeJsonAttachment :: EncodeJson Attachment where
  encodeJson (Attachment r)
    =  "fallback" := r.fallback
    ~> "color" := r.color
    ~> "pretext" := r.pretext
    ~> "author_name" := r.authorName
    ~> "author_link" := r.authorLink
    ~> "author_icon" := r.authorIcon
    ~> "title" := r.title
    ~> "title_link" := r.titleLink
    ~> "text" := r.text
    ~> "fields" := r.fields
    ~> "image_url" := r.imageUrl
    ~> "thumb_url" := r.thumbUrl
    ~> "footer" := r.footer
    ~> "footer_icon" := r.footerIcon
    ~> "ts" := r.ts
    ~> jsonEmptyObject
