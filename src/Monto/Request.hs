{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.Request where

import Data.Aeson.TH

import qualified Data.Set as S

import Monto.Types
import Monto.VersionMessage (VersionMessage)
import Monto.ProductMessage (ProductMessage)

data Message = VersionMessage VersionMessage | ProductMessage ProductMessage
  deriving (Eq,Ord,Show)
$(deriveJSON defaultOptions ''Message)

data Request = Request
    { source :: Source
    , serviceID :: ServiceID
    , requirements :: [Message]}
  deriving (Ord,Show)

instance Eq Request where
  Request src1 service1 msgs1 == Request src2 service2 msgs2
      = src1 == src2
     && service1 == service2
     && S.fromList msgs1 == S.fromList msgs2

$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label -> label
}) ''Request)
