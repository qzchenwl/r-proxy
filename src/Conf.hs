{-# LANGUAGE OverloadedStrings #-}

module Conf
( Conf(..)
, Rule(..)
, decodeFile
) where

import qualified Prelude
import ClassyPrelude
import Text.Regex.PCRE (Regex, makeRegex)
import Data.Yaml (FromJSON(..), Value(..), (.:), (.:?), decodeFile)
import Data.Either.Utils (fromRight)

data Conf = Conf { rules :: [ Rule ] } deriving (Show)

data Rule = Rule { unRegex :: Regex
                 , unRegexStr :: Text
                 , unUrl :: Maybe Text
                 , unHost :: Maybe Text
                 , unPort :: Maybe Int
                 , unIp :: Maybe Text
                 }

instance Show Rule where
    show (Rule _ regex url host port ip) = intercalate "\n" lines
        where lines = catMaybes [ ("regex = " <>) . show <$> Just regex
                                , ("url   = " <>) . show <$> url
                                , ("host  = " <>) . show <$> host
                                , ("port  = " <>) . show <$> port
                                , ("ip    = " <>) . show <$> ip
                                ]

instance FromJSON Conf where
    parseJSON (Object v) = Conf <$> v .: "rules"

instance FromJSON Rule where
    parseJSON (Object v) = Rule <$> (makeRegex :: String -> Regex) <$> v .: "regex"
                                <*> v .: "regex"
                                <*> v .:? "url"
                                <*> v .:? "host"
                                <*> v .:? "port"
                                <*> v .:? "ip"


