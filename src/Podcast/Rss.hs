
{-# LANGUAGE ScopedTypeVariables #-}

module Podcast.Rss ( createConnManager
                   , fetchRss
                   , fetchEpisode
                   ) where

import Podcast.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Exception
import Control.Monad
import Control.Concurrent


fetchEpisode :: Manager
             -> (B.ByteString -> IO ())
             -> Episode
             -> IO (Either HttpException ())
fetchEpisode mgr chunkHandler (Episode _ (Link url)) = execRequest mgr chunkHandler url

fetchRss :: Manager -> Feed -> IO (Either HttpException RssFeedXML)
fetchRss mgr (Feed feed) = do
  resp <- simpleRequest mgr feed
  return $ either Left (Right . makeRss) resp
  where simpleRequest :: Manager -> String-> IO (Either HttpException LB.ByteString)
        simpleRequest manager url = exc2either $ do
          req <- parseRequest url
          resp <- httpLbs req manager
          return $ responseBody resp


-- Helper functions

retry :: forall a b. Exception a => Int -> a -> IO b -> IO (Either a ())
retry 0 err _ = return $ Left err
retry times err action = do
  result <- try action :: IO (Either IOError b)
  case result of
    Left _ -> threadDelay 1000 >> retry (times - 1) err action
    Right _ -> return $ Right ()

exc2either :: forall a b. Exception a => IO b -> IO (Either a b)
exc2either action = catch (Right `liftM` action) (return . Left)

createConnManager :: IO Manager
createConnManager = newManager tlsManagerSettings

execRequest :: Manager
              -> (B.ByteString -> IO ())
              -> String
              -> IO (Either HttpException ())
execRequest mgr chunkHandler url = retry 10 err execRequest'
  where err = InvalidUrlException url "Failed to fetch URL multiple times."
        execRequest' :: IO (Either HttpException ())
        execRequest' = exc2either $ do
          req <- parseRequest url
          withResponse req mgr responseHandler
        responseHandler :: Response BodyReader -> IO ()
        responseHandler response = do
          chunk <- brRead $ responseBody response
          if B.null chunk
            then return ()
            else chunkHandler chunk >> responseHandler response
