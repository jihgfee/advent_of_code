{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson;
import Data.Maybe;
import Data.Map;
import Data.Text;
import Data.ByteString.Lazy;

import GHC.Generics

data CompletionTime = CompletionTime {
      time :: String
    } deriving (Generic, Show)

data CompletionDay = CompletionDay {
      p1 :: Maybe CompletionTime,
      p2 :: Maybe CompletionTime
    } deriving (Generic, Show)
data Member = Member {
      id :: String,
      name :: String,
      completionDays :: Map String CompletionDay
    } deriving (Generic, Show)
data Root = Root {
      members :: Map String Member
    } deriving (Generic, Show)

instance FromJSON CompletionTime where
    parseJSON = withObject "CompletionTime" $ \v -> CompletionTime
        <$> v .: "get_star_ts"
instance FromJSON CompletionDay where
    parseJSON = withObject "CompletionDay" $ \v -> CompletionDay
        <$> v .:? "1"
        <*> v .:? "2"
instance FromJSON Member where
    parseJSON = withObject "Member" $ \v -> Member
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "completion_day_level"
instance FromJSON Root where
    parseJSON = withObject "Root" $ \v -> Root
        <$> v .: "members"




main = do
  input <- Data.ByteString.Lazy.getContents
  print ((fromJust ((decode input) :: Maybe Root)))
