
{-# LANGUAGE ScopedTypeVariables #-}

module Podcast.Rss ( createConnManager
                   , fetchRss
                   , fetchEpisode
                   ) where

import Podcast.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Exception
type AudioFile = B.ByteString


exc2either :: forall a b. Exception a => IO b -> IO (Either a b)
exc2either action = catch action' (return . Left)
  where action' :: IO (Either a b)
        action' = do
          result <- action
          return $ ((Right result) :: Either a b)

createConnManager :: IO Manager
createConnManager = newManager tlsManagerSettings

simpleRequest :: Manager -> String -> IO (Either HttpException B.ByteString)
simpleRequest mgr url = exc2either $ do
  req <- parseRequest url
  resp <- httpLbs req mgr
  return $ responseBody resp

fetchRss :: Manager -> Feed -> IO (Either HttpException RssFeedXML)
fetchRss mgr (Feed feed) = do
  resp <- simpleRequest mgr feed
  return $ either Left (Right . makeRss) resp

fetchEpisode :: Manager -> Episode -> IO (Either HttpException AudioFile)
fetchEpisode mgr (Episode _ (Link url)) = simpleRequest mgr url
