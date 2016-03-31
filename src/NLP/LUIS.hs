{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module NLP.LUIS ( -- * Querying
                      query
                    , queryExc
                    , Credentials(..)
                      -- * Response Type
                    , Response
                    , responseQuery
                    , responseIntents
                    , responseEntities
                      -- * Intent Type
                    , Intent
                    , intentType
                    , intentScore
                      -- * Entity Type
                    , Entity
                    , entityType
                    , entityScore
                    , entityText
                    , entityStartIndex
                    , entityEndIndex
                      -- * Errors
                    , LUISError(..)
                    ) where

import Control.Exception (Exception(..), fromException, try)
import Data.Data (Data)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Control.Lens
import Data.Aeson
import Data.Text (Text)
import Data.Vector (Vector)
import Network.HTTP.Client (HttpException)
import Network.Wreq hiding (Response)

data Intent = Intent { _intentType :: !Text
                     , _intentScore :: !Double
                     } deriving (Eq, Show, Read, Typeable, Data, Generic)

makeLenses ''Intent

instance FromJSON Intent where
  parseJSON = withObject "intent" $ \o ->
    Intent <$> o .: "intent" <*> o .: "score"

data Entity = Entity { _entityText :: !Text
                     , _entityType :: !Text
                     , _entityStartIndex :: !Int
                     , _entityEndIndex :: !Int
                     , _entityScore :: !Double
                     } deriving (Eq, Show, Read, Typeable, Data, Generic)

makeLenses ''Entity

instance FromJSON Entity where
  parseJSON = withObject "entity" $ \o -> do
    _entityText <- o .: "entity"
    _entityType <- o .: "type"
    _entityStartIndex <- o .: "startIndex"
    _entityEndIndex <- o .: "endIndex"
    _entityScore <- o .: "score"
    pure Entity{..}

data Response = Response { _responseQuery :: !Text
                         , _responseIntents :: !(Vector Intent)
                         , _responseEntities :: !(Vector Entity)
                         } deriving (Eq, Show, Read, Typeable, Data, Generic)

makeLenses ''Response

instance FromJSON Response where
  parseJSON = withObject "luisResponse" $ \o ->
    Response <$> o .: "query" <*> o .: "intents" <*> o .: "entities"

data LUISError = HttpError HttpException
               | ResponseError JSONError
               deriving (Show, Typeable, Generic)

instance Exception LUISError where
  fromException e = if isJust he then he else je
    where he = HttpError <$> fromException e
          je = ResponseError <$> fromException e

data Credentials = Credentials
                   { applicationId :: !Text
                   , subscriptionKey :: !Text
                   } deriving (Show, Read, Eq, Typeable, Data, Generic)

queryExc :: Credentials -> Text -> IO Response
queryExc Credentials{..} str = do
  let opts = defaults & param "id" .~ [applicationId]
                      & param "subscription-key" .~ [subscriptionKey]
                      & param "q" .~ [str]
  resp <- getWith opts "https://api.projectoxford.ai/luis/v1/application"
  luisResp <- asJSON resp
  return (luisResp ^. responseBody)

query :: Credentials -> Text -> IO (Either LUISError Response)
query creds str = try (queryExc creds str)
