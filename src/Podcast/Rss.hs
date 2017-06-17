
{-# LANGUAGE ScopedTypeVariables #-}

module Podcast.Rss ( createConnManager
                   , streamToFile
                   , fetchRss
                   , fetchEpisode
                   ) where

import Podcast.Types
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Exception
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Resource.Internal (ResourceT)

type EpisodeSink = Sink B.ByteString (ResourceT IO) ()


fetchEpisode :: Manager
             -> EpisodeSink
             -> Episode
             -> IO (Either HttpException ())
fetchEpisode mgr chunkHandler (Episode _ (Link url) _) = execRequest mgr chunkHandler url

fetchRss :: Manager -> Feed -> IO (Either HttpException RssFeedXML)
fetchRss mgr (Feed feed) = do
  resp <- simpleRequest mgr feed
  return $ either Left (Right . makeRss) resp
  where simpleRequest :: Manager -> String -> IO (Either HttpException LB.ByteString)
        simpleRequest manager url = try $ do
          req <- parseUrlThrow url
          resp <- httpLbs req manager
          return $ responseBody resp


-- Helper functions

createConnManager :: IO Manager
createConnManager = newManager $ tlsManagerSettings

streamToFile :: FilePath -> EpisodeSink
streamToFile file = sinkFile file

execRequest :: Manager-> EpisodeSink -> String -> IO (Either HttpException ())
execRequest mgr chunkHandler url = (try $ do
            request <- parseUrlThrow url
            runResourceT $ do
                response <- http request mgr
                responseBody response $$+- chunkHandler) `catch` \(e :: IOException) -> (putStrLn $ "FFS: " ++ show e) >> (return $ Left (InvalidUrlException (show e) (show e)))
