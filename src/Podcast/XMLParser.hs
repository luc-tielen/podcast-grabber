
{-# LANGUAGE Arrows #-}

module Podcast.XMLParser ( parseRss ) where

import Podcast.Types
import Podcast.DB (parsePublishDate)
import Text.XML.HXT.Core
import Data.ByteString.Lazy.Char8


atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep $ isElem >>> hasName tag

parseRss :: RssFeedXML -> [Episode]
parseRss rss = runLA (xread >>> xmlParser) $ getRss' rss
  where xmlParser :: ArrowXml a => a XmlTree Episode
        xmlParser = proc xml -> do
          item <- atTag "item" -< xml
          titleTag <- atTag "title" -< item
          title <- deep $ getText -< titleTag
          dateTag <- atTag "pubDate" -< item
          date <- deep $ getText -< dateTag
          episode <- atTag "enclosure" -< item
          url <- getAttrValue "url" -< episode
          returnA -< Episode (Title title) (Link url) (parsePublishDate date)
        getRss' = unpack . getRss
