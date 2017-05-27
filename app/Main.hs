
module Main where

import Lib
import Options.Applicative
import System.Directory
import Data.Map


main :: IO ()
main = do
  dir <- dbDir
  file <- dbFile
  isFirstTime <- isRunningForFirstTime dir file
  case isFirstTime of
    True -> do
      setupInitialDB dir file
      doMain
    False -> doMain
    where isRunningForFirstTime d f = do
            dirExists <- doesDirectoryExist d
            fileExists <- doesFileExist f
            return . not $ dirExists && fileExists
          setupInitialDB d f = do
            createDBDir d
            createInitialDB f
          createDBDir d = createDirectoryIfMissing False d
          createInitialDB file = writeDB file initialDB
          initialDB = DB { feeds = [], episodes = fromList [] }

doMain :: IO ()
doMain = do
  db <- dbFile
  processFlags db =<< execParser parseOptions
  where processFlags db (AddPodcast f) = addFeed db f
        processFlags db (RemovePodcast f) = removeFeed db f
        processFlags db Update = updateFeeds db
