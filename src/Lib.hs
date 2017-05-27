
module Lib ( module Podcast.Types
           , module Podcast.OptionParser
           , dbDir
           , dbFile
           , addFeed
           , removeFeed
           , updateFeeds
           ) where

import Podcast.Types
import Podcast.OptionParser
import Podcast.XMLParser
import Podcast.Rss
import Network.HTTP.Client
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import System.Directory
import System.FilePath
import Data.Map (delete, fromListWith, unionWith)
import Data.Maybe
import qualified Data.Map as Map (lookup)
import qualified Data.ByteString.Lazy as B


data FetchResult = FetchFeedError Feed HttpException
                 | FetchEpisodeError Feed Episode HttpException
                 | SkippedOldEpisode Feed Episode
                 | FetchSuccess Feed Episode
                 deriving (Show)

instance Eq FetchResult where
  (FetchFeedError feed1 _) == (FetchFeedError feed2 _) = feed1 == feed2
  (FetchEpisodeError feed1 ep1 _) == (FetchEpisodeError feed2 ep2 _) = feed1 == feed2 && ep1 == ep2
  (SkippedOldEpisode feed1 ep1) == (SkippedOldEpisode feed2 ep2) = feed1 == feed2 && ep1 == ep2
  (FetchSuccess feed1 ep1) == (FetchSuccess feed2 ep2) = feed1 == feed2 && ep1 == ep2
  _ == _ = False


dbDir :: IO FilePath
dbDir = do
  homeDir <- getHomeDirectory
  return $ homeDir </> ".podcast-grabber"

dbFile :: IO FilePath
dbFile = do
  dir <- dbDir
  return $ dir </> "feedDB.json"


addFeed :: FilePath -> Feed -> IO ()
addFeed db f = do
  feedDB <- readDB db
  case feedDB of
    Left err -> do
      putStrLn "Failed to add feed to list of RSS feeds in DB:"
      putStrLn err
    Right feedDB' -> do
      let feedDB'' = feedDB' { feeds = f : (feeds feedDB') }
      writeDB db feedDB''


removeFeed :: FilePath -> Feed -> IO ()
removeFeed db f = do
  feedDB <- readDB db
  case feedDB of
    Left err -> do
      putStrLn "Failed to remove feed from list of RSS feeds in DB:"
      putStrLn err
    Right feedDB' -> do
      let feedDB'' = removeFeed' f feedDB'
      writeDB db feedDB''

removeFeed' :: Feed -> FeedDB -> FeedDB
removeFeed' feed feedDB = feedDB'
  where feedDB' = feedDB { feeds = feeds', episodes = episodes' }
        feeds' = filter (\f -> feed /= f) $ feeds feedDB
        episodes' = delete feed (episodes feedDB)


updateFeeds :: FilePath ->  IO ()
updateFeeds f = do
  db <- readDB f
  case db of
    Left err -> do
      putStrLn "Failed to fetch feeds out of DB: "
      putStrLn err
    Right feedDB -> do
      mgr <- createConnManager
      feedQueue <- atomically mkQueue
      let fs = feeds feedDB
      let numFeeds = length fs
      if numFeeds == 0
        then return ()
        else do
          fetchFeeds mgr feedQueue feedDB fs
          fetchResults <- atomically $ collectFromQueue numFeeds feedQueue
          handleUpdateResults f feedDB fetchResults

handleUpdateResults :: FilePath -> FeedDB -> [(Feed, [FetchResult])] -> IO ()
handleUpdateResults f oldDB fetchResults = do
  mapM_ fmtError badResults
  writeDB f newDB
  where newDB = updateFeedDB goodResults oldDB
        (badResults, goodResults) = break isSuccess results
        results = concatMap snd fetchResults
        isSuccess (FetchSuccess _ _) = True
        isSuccess _                  = False
        fmtError (FetchFeedError (Feed feed) err) = do
          putStrLn $ "Failed to fetch feed: " ++ feed
          putStrLn $ show err
        fmtError (FetchEpisodeError (Feed feed) (Episode (Title title) _) err) = do
          putStrLn $ "Failed to fetch episode: " ++ title ++ " for feed: " ++ feed
          putStrLn $ show err
        fmtError (SkippedOldEpisode (Feed feed) (Episode (Title title) _)) = do
          putStrLn $ "Skipped previously fetched episode: " ++ title ++ " for feed: " ++ feed
        fmtError _ = return ()

updateFeedDB :: [FetchResult] -> FeedDB -> FeedDB
updateFeedDB results oldDB = newDB
  where newDB = oldDB { episodes = updatedEps }
        oldEps = episodes oldDB
        updatedEps = (fromListWith (++) $ map resultToKV results) `mergeMaps` oldEps
        a `mergeMaps` b = unionWith (++) a b
        resultToKV (FetchSuccess feed episode) = (feed, [episode])
        resultToKV (SkippedOldEpisode feed episode) = (feed, [episode])
        resultToKV _ = error "Unreachable!"

fetchFeeds :: Manager -> TVar [(Feed, [FetchResult])] -> FeedDB -> [Feed] -> IO ()
fetchFeeds mgr feedQueue db fs = mapM_ (fetchFeed mgr feedQueue db) fs

fetchFeed :: Manager -> TVar [(Feed, [FetchResult])] -> FeedDB -> Feed -> IO ThreadId
fetchFeed mgr feedQueue db f = forkIO $ do
  rssResult <- fetchRss mgr f
  case rssResult of
    Left err -> atomically $ addToQueue [(f, [FetchFeedError f err])] feedQueue
    Right rss -> do
        let eps = parseRss rss
        let numEps = length eps
        epQueue <- atomically mkQueue
        mapM_ (fetchEp db f mgr epQueue) eps
        results <- atomically $ collectFromQueue numEps epQueue
        atomically $ addToQueue [(f, results)] feedQueue

fetchEp :: FeedDB -> Feed -> Manager -> TVar [FetchResult] -> Episode -> IO ThreadId
fetchEp db feed mgr queue ep@(Episode (Title title) _) = forkIO $ do
  isOld <- isOldEpisode db feed ep
  if isOld
    then do atomically $ addToQueue [SkippedOldEpisode feed ep] queue
    else do
      putStrLn $ "Fetching episode: " ++ title
      audioResult <- fetchEpisode mgr ep
      case audioResult of
        Left err -> atomically $ addToQueue [FetchEpisodeError feed ep err] queue
        Right audio -> do
          dir <- dbDir
          B.writeFile (dir </> getEpisodeFilePath ep) audio
          atomically $ addToQueue [FetchSuccess feed ep] queue


getEpisodeFilePath :: Episode -> FilePath
getEpisodeFilePath (Episode (Title title) _) = escape $ title ++ ".mp3"
  where escape = map escape'
        escape' ' ' = '_'
        escape' '/' = '_'
        escape' c = c

isOldEpisode :: FeedDB -> Feed -> Episode -> IO Bool
isOldEpisode feedDB feed episode = do
  let isInDB = checkIfInDB feed episode feedDB
  isOld <- checkIfAlreadyDownloaded episode
  return $ isInDB || isOld
  where checkIfInDB f ep (DB _ db) = isJust $ do
          episodeList <- Map.lookup f db
          return $ elem ep episodeList
        checkIfAlreadyDownloaded ep = do
          dir <- dbDir
          doesFileExist $ dir </> getEpisodeFilePath ep


mkQueue :: STM (TVar [a])
mkQueue = newTVar []

collectFromQueue :: Int -> TVar [a] -> STM [a]
collectFromQueue amount queue = do
  xs <- readTVar queue
  check (length xs == amount)
  return xs

addToQueue :: [a] -> TVar [a] -> STM ()
addToQueue xs queue = do
  ys <- readTVar queue
  let zs = xs ++ ys
  writeTVar queue zs