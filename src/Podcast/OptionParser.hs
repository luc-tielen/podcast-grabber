
module Podcast.OptionParser ( parseOptions ) where

import Podcast.Types
import Options.Applicative
import Data.Semigroup ((<>))



parseAddPodcast, parseRemovePodcast, parseUpdate :: Parser CmdLineFlags

parseAddPodcast = AddPodcast . Feed <$> strOption params
  where params = long "add"
              <> metavar "FEED"
              <> help "Episode to the feed you want to add."
parseRemovePodcast = RemovePodcast . Feed <$> strOption params
  where params = long "rm"
              <> metavar "FEED"
              <> help "Episode to the feed you want to remove."
parseUpdate = switch params *> pure Update
  where params = long "update"
              <> help "Update the local podcast DB by fetching new episodes and marking others as old."

parseOptions :: ParserInfo CmdLineFlags
parseOptions = info (parser <**> helper) helpInfo
  where parser = parseAddPodcast
              <|> parseRemovePodcast
              <|> parseUpdate
        helpInfo = fullDesc
                <> progDesc "Manage podcasts via the commandline."
                <> header (progName ++ " - An utility for managing podcasts via the commandline.")
        progName = "podcast-grabber"
