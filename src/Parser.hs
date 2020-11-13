module Parser where

data TvShow = TvShow { showID :: Int
                     , showName :: String
                     , language :: String
                     , runtime :: Int
                     , premierDate :: String
                     , genre :: [String]
                     , url :: String
                     , showSummary :: String
                     } deriving (Show)


data Season = Season { seasonID :: Int
                     , seaShowID :: Int
                     , seasonNum :: Int
                     , numEpisodes :: Int
                     , startDate :: String
                     , endDate :: String
                     } deriving (Show)


data Episode = Episode { episodeID :: Int
                       , epiShowID :: Int
                       , epiSeasonID :: Int
                       , episodeNum :: Int
                       , episodeName :: String
                       , episodeSummary :: String
                       , airDate :: String
                       } deriving (Show)


data Cast = Cast { castID :: Int
                 , castShowID :: Int
                 , actor :: String
                 , character :: String
                 } deriving (Show)

