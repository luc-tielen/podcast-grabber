
{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns #-}

module Podcast.DB ( Feed(..)
                  , Title(..)
                  , Link(..)
                  , Episode(..)
                  , EpisodeMap
                  , FeedDB(..)
                  , parsePublishDate
                  , readDB
                  , writeDB
                  ) where


import qualified Data.ByteString.Lazy as B
import Data.Map.Strict
import Data.Aeson
import GHC.Generics
import Data.Time.Format
import qualified Data.Time.Calendar as C


newtype Feed = Feed String deriving (Eq, Ord, Show, Generic)
newtype Title = Title String deriving (Eq, Show, Generic)
newtype Link = Link String deriving (Eq, Show, Generic)
newtype Day = Day Int deriving (Eq, Show, Generic)
newtype Month = Month Int deriving (Eq, Show, Generic)
newtype Year = Year Integer deriving (Eq, Show, Generic)
data PublishDate = PublishDate !Day !Month !Year deriving (Eq, Generic)
data Episode = Episode !Title !Link !PublishDate deriving (Eq, Show, Generic)

type EpisodeMap = Map Feed [Episode]
type Timestamp = String

data FeedDB = DB { feeds :: ![Feed]
                 , episodes :: !EpisodeMap
                 } deriving (Eq, Show, Generic)


instance Show PublishDate where
  show (PublishDate (Day d) (Month m) (Year y)) = show d ++ "_" ++ show m ++ "_" ++ show y

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
instance FromJSON Day
instance ToJSON Day
instance FromJSON Month
instance ToJSON Month
instance FromJSON Year
instance ToJSON Year
instance FromJSON PublishDate
instance ToJSON PublishDate
instance FromJSON Episode
instance ToJSON Episode


-- NOTE: this expects an RFC822 timestamp!
parsePublishDate :: Timestamp -> PublishDate
parsePublishDate ts = pubDate
  where time = parseTimeM True defaultTimeLocale rfc822DateFormat ts :: Maybe C.Day
        Just (year, month, day) = fmap C.toGregorian time
        pubDate = PublishDate (Day day) (Month month) (Year year)


readDB :: FilePath -> IO (Either String FeedDB)
readDB dbFile = eitherDecode <$> B.readFile dbFile

writeDB :: FilePath -> FeedDB -> IO ()
writeDB dbFile db = B.writeFile dbFile $ encode db
