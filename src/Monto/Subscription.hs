module Monto.Subscription where

import           Data.Text(Text)

import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import           Monto.Types (toText)


data Subscription = Source | Product | Language | ServiceID
  deriving(Show,Read)

type Topic = [Subscription]

subscription :: ProductMessage -> Subscription -> Text
subscription prod sub = case sub of
  Source -> toText $ P.source prod
  Product -> toText $ P.product prod
  Language -> toText $ P.language prod
  ServiceID -> toText $ P.serviceID prod

topic :: ProductMessage -> Topic -> [Text]
topic prod top = subscription prod <$> top
