{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseTvShow,
    parseTvShowFromSearch,
    parseSeason,
    parseEpisode,
    parseCast,
    updateShowId,
    updateEpisodeId,
    updateCastId,
    getTvShowId,
    TvShow (showID, showName, language, runtime, premiered, genres, url, showSummary),
    TvShowFromSearch (show),
    Season (seasonID, seaShowID, seasonNum, numEpisodes, startDate, endDate),
    Episode (episodeID, epiShowID, epiSeasonID, episodeNum, episodeName, episodeSummary, airDate),
    Cast (castID, castShowID, actor, character),
  )
where

import Control.Monad (MonadPlus (mzero))
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON,
    Value (Object),
    eitherDecode,
    (.:),
  )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics (Generic)
import Data.Time

data TvShow = TvShow
  { showID :: Int,
    showName :: String,
    language :: String,
    runtime :: Int,
    premiered :: String,
    genres :: [String],
    url :: String,
    showSummary :: String
  }
  deriving (Show, Generic)

data TvShowFromSearch = TvShowFromSearch {show :: TvShow}
  deriving (Show, Generic)

instance FromJSON TvShowFromSearch where
  parseJSON (Object v) =
    TvShowFromSearch <$> (v .: "show")
  parseJSON _ = mzero

instance FromJSON TvShow where
  parseJSON (Object v) =
    TvShow <$> (v .: "id")
      <*> (v .: "name")
      <*> (v .: "language")
      <*> (v .: "runtime")
      <*> (v .: "premiered")
      <*> (v .: "genres")
      <*> (v .: "officialSite")
      <*> (v .: "summary")
  parseJSON _ = mzero

getTvShowId :: TvShow -> Int
getTvShowId tvShow = showID tvShow

instance ToJSON TvShow

parseTvShow :: L8.ByteString -> Either String TvShow
parseTvShow json = eitherDecode json :: Either String TvShow

parseTvShowFromSearch :: L8.ByteString -> Either String TvShowFromSearch
parseTvShowFromSearch json = eitherDecode json :: Either String TvShowFromSearch

data Season = Season
  { seasonID :: Int,
    seaShowID :: Int,
    seasonNum :: Int,
    numEpisodes :: Int,
    startDate :: Day,
    endDate :: Day
  }
  deriving (Show, Generic)

updateShowId :: Season -> Int -> Season
updateShowId season showId = season {seaShowID = showId}

instance FromJSON Season where
  parseJSON (Object v) =
    Season <$> (v .: "id")
      <*> (v .: "id")
      <*> (v .: "number")
      <*> (v .: "episodeOrder")
      <*> (v .: "premiereDate")
      <*> (v .: "endDate")
  parseJSON _ = mzero

instance ToJSON Season

parseSeason :: L8.ByteString -> Either String Season
parseSeason json = eitherDecode json :: Either String Season

data Episode = Episode
  { episodeID :: Int,
    epiShowID :: Int,
    epiSeasonID :: Int,
    episodeNum :: Int,
    episodeName :: String,
    episodeSummary :: String,
    airDate :: Day
  }
  deriving (Show, Generic)

updateEpisodeId :: Episode -> Int -> Int -> Episode
updateEpisodeId episode showId seasonId = episode {epiShowID = showId, epiSeasonID = seasonId}

instance FromJSON Episode where
  parseJSON (Object v) =
    Episode <$> (v .: "id")
      <*> (v .: "id")
      <*> (v .: "season")
      <*> (v .: "number")
      <*> (v .: "name")
      <*> (v .: "summary")
      <*> (v .: "airDate")
  parseJSON _ = mzero

instance ToJSON Episode

parseEpisode :: L8.ByteString -> Either String Episode
parseEpisode json = eitherDecode json :: Either String Episode

data Cast = Cast
  { castID :: Int,
    castShowID :: Int,
    actor :: String,
    character :: String
  }
  deriving (Show, Generic)

updateCastId :: Cast -> Int -> Cast
updateCastId cast showId = cast {castShowID = showId}

instance FromJSON Cast where
  parseJSON (Object v) =
    Cast <$> (v .: "id")
      <*> (v .: "id")
      <*> ((v .: "person") >>= (.: "name"))
      <*> ((v .: "character") >>= (.: "name"))
  parseJSON _ = mzero

instance ToJSON Cast

parseCast :: L8.ByteString -> Either String Cast
parseCast json = eitherDecode json :: Either String Cast