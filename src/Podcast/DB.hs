
{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns #-}

module Podcast.DB ( Feed(..)
                  , Title(..)
                  , Link(..)
                  , Episode(..)
                  , EpisodeMap
                  , FeedDB(..)
                  , readDB
                  , writeDB
                  ) where


import qualified Data.ByteString.Lazy as B
import Data.Map.Strict
import Data.Aeson
import GHC.Generics


newtype Feed = Feed String deriving (Eq, Ord, Show, Generic)
newtype Title = Title String deriving (Eq, Show, Generic)
newtype Link = Link String deriving (Eq, Show, Generic)
data Episode = Episode !Title !Link deriving (Eq, Show, Generic)

type EpisodeMap = Map Feed [Episode]

data FeedDB = DB { feeds :: ![Feed]
                 , episodes :: !EpisodeMap
                 } deriving (Eq, Show, Generic)


instance FromJSON FeedDB
instance ToJSON FeedDB
instance FromJSON Feed
instance ToJSON Feed
instance FromJSONKey Feed
instance ToJSONKey Feed
instance FromJSON Link
instance ToJSON Link
instance FromJSON Title
instance ToJSON Title
instance FromJSON Episode
instance ToJSON Episode


readDB :: FilePath -> IO (Either String FeedDB)
readDB dbFile = eitherDecode <$> B.readFile dbFile

writeDB :: FilePath -> FeedDB -> IO ()
writeDB dbFile db = B.writeFile dbFile $ encode db
