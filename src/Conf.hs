{-# LANGUAGE OverloadedStrings #-}

module Conf
( Conf(..)
, Rule(..)
, decodeFile
) where

import qualified Prelude
import ClassyPrelude
import Text.Regex.PCRE.Heavy (Regex, compileM)
import Data.Yaml (FromJSON(..), Value(..), (.:), (.:?), decodeFile)
import Data.Either.Utils (fromRight)

data Conf = Conf { rules :: [ Rule ] } deriving (Show)

data Rule = Rule { unRegex :: Regex
                 , unUrl :: Maybe Text
                 , unHost :: Maybe Text
                 , unPort :: Maybe Int
                 , unIp :: Maybe Text
                 } deriving (Show)

instance FromJSON Conf where
    parseJSON (Object v) = Conf <$> v .: "rules"

instance FromJSON Rule where
    parseJSON (Object v) = Rule <$> (fromRight . \s -> compileM (encodeUtf8 s) []) <$> v .: "regex"
                                <*> v .:? "url"
                                <*> v .:? "host"
                                <*> v .:? "port"
                                <*> v .:? "ip"


