{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Monto.Request where

import           Data.Aeson
import           Data.Aeson.Casing    (snakeCase)
import           Data.Aeson.TH
import           Data.Maybe
import qualified Data.Set             as S

import           Monto.ProductMessage (ProductMessage)
import           Monto.Source
import           Monto.SourceMessage  (SourceMessage)
import           Monto.Types

data Message = SourceMessage SourceMessage | ProductMessage ProductMessage
  deriving (Eq,Ord,Show)

instance ToJSON Message where
  toJSON (SourceMessage sm) = toJSON sm
  toJSON (ProductMessage pm) = toJSON pm

instance FromJSON Message where
  parseJSON = withObject "message has to be a json object" $ \obj -> do
    prod <- obj .:? "product"
    if isJust (prod :: Maybe Product)
       then ProductMessage <$> parseJSON (Object obj)
       else SourceMessage <$> parseJSON (Object obj)

data Request = Request
    { source       :: Source
    , serviceID    :: ServiceID
    , requirements :: [Message]
    }
  deriving (Ord,Show)

instance Eq Request where
  Request src1 sid1 msgs1 == Request src2 sid2 msgs2
      = src1 == src2
     && sid1 == sid2
     && S.fromList msgs1 == S.fromList msgs2

$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''Request)

