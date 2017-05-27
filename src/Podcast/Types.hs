
module Podcast.Types ( module Podcast.DB
                     , CmdLineFlags(..)
                     , RssFeedXML
                     , makeRss
                     , getRss) where

import Podcast.DB
import qualified Data.ByteString.Lazy.Char8 as B

-- Value does not contain top level XML encoding tag
newtype RssFeedXML = Rss { getRss :: B.ByteString } deriving Show


makeRss :: B.ByteString -> RssFeedXML
makeRss s = Rss . dropXMLTag $ s
  where dropXMLTag = B.unlines . (drop 1) . B.lines

data CmdLineFlags = AddPodcast Feed
                  | RemovePodcast Feed
                  | Update
                  deriving Show
