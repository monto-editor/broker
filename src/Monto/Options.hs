{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Monto.Options where

import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Data.Text           (Text)

import           Monto.Types


data Option
  = BoolOption
    { optionID               :: OptionID
    , label                  :: Text
    , boolOptionDefaultValue :: Bool
    }
  | NumberOption
    { optionID                 :: OptionID
    , label                    :: Text
    , numberOptionDefaultValue :: Int
    , from                     :: Int
    , to                       :: Int
    }
  | TextOption
    { optionID               :: OptionID
    , label                  :: Text
    , textOptionDefaultValue :: Text
    , regularExpression      :: Text
    }
  | XorOption
    { optionID              :: OptionID
    , label                 :: Text
    , xorOptionDefaultValue :: Text
    , values                :: [Text]
    }
  | GroupOption
    { requiredOption :: OptionID
    , members        :: [Option]
    }

instance ToJSON Option where
  toJSON opt =
    case opt of
      GroupOption req mem -> object
        [ "required_option" .= req
        , "members" .= mem
        ]
      _ -> object $
        [ "option" .= optionID opt
        , "label" .= label opt
        ] ++ case opt of
          BoolOption {boolOptionDefaultValue = def} ->
            [ "type" .= ("boolean" :: Text)
            , "default_value" .= def
            ]
          NumberOption {numberOptionDefaultValue=def,from,to} ->
            [ "type" .= ("number" :: Text)
            , "default_value" .= def
            , "from" .= from
            , "to" .= to
            ]
          TextOption {textOptionDefaultValue=def,regularExpression} ->
            [ "type" .= ("text" :: Text)
            , "default_value" .= def
            , "regular_expression" .= regularExpression
            ]
          XorOption {xorOptionDefaultValue=def,values} ->
            [ "type" .= ("xor" :: Text)
            , "default_value" .= def
            , "values" .= values
            ]
          _ -> []

instance FromJSON Option where
  parseJSON = withObject "Option has to be an object" $ \obj ->
    case M.lookup "type" obj of
      Just "boolean" ->
        BoolOption
          <$> obj .: "option_id"
          <*> obj .: "label"
          <*> obj .: "default_value"
      Just "number" ->
        NumberOption
          <$> obj .: "option_id"
          <*> obj .: "label"
          <*> obj .: "default_value"
          <*> obj .: "from"
          <*> obj .: "to"
      Just "text" ->
        TextOption
          <$> obj .: "option_id"
          <*> obj .: "label"
          <*> obj .: "default_value"
          <*> obj .: "regular_expression"
      Just "xor" ->
        XorOption
          <$> obj .: "option_id"
          <*> obj .: "label"
          <*> obj .: "default_value"
          <*> obj .: "values"
      Just _ -> fail "unrecognized option type"
      Nothing ->
        GroupOption
          <$> obj .: "option_id"
          <*> obj .: "members"
