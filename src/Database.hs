module Database
  ( initialiseDB
  , saveTvShow
  ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Parser

initialiseDB :: IO Connection
initialiseDB =
  do
    conn <- connectSqlite3 "tvmaze.sqlite"
    run conn "CREATE TABLE IF NOT EXISTS TvShow (\
      \showID INT NOT NULL PRIMARY KEY, \
      \showName VARCHAR(50) NOT NULL, \
      \language VARCHAR(30), \
      \runtime INT, \
      \premiered DATE, \
      \url VARCHAR(100), \
      \showSummary VARCHAR(1000) \
      \)"
      []
    commit conn
    print "Table TvShow has been created"
    conn <- connectSqlite3 "tvmaze.sqlite"
    run conn "CREATE TABLE IF NOT EXISTS Cast (\
      \castID INT NOT NULL PRIMARY KEY, \
      \castShowID NOT NULL, \
      \actor VARCHAR(50), \
      \FOREIGN KEY(castShowID) REFERENCES TvShow(showID) \
      \)"
      []
    commit conn
    print "Table Cast has been created"
    conn <- connectSqlite3 "tvmaze.sqlite"
    run conn "CREATE TABLE IF NOT EXISTS Season (\
      \seasonID INT NOT NULL PRIMARY KEY, \
      \seaShowID INT NOT NULL, \
      \seasonNum INT NOT NULL, \
      \numEpisodes INT NOT NULL, \
      \startDate DATE, \
      \endDate DATE, \
      \FOREIGN KEY(seaShowID) REFERENCES TvShow(showID) \
      \)"
      []
    commit conn
    print "Table Season has been created"
    conn <- connectSqlite3 "tvmaze.sqlite"
    run conn "CREATE TABLE IF NOT EXISTS Episode (\
      \episodeID INT NOT NULL PRIMARY KEY, \
      \epiShowID INT NOT NULL, \
      \epiSeasonID INT NOT NULL, \
      \episodeNum INT NOT NULL, \
      \episodeName VARCHAR(100), \
      \episodeSummary VARCHAR(1000), \
      \airDate DATE, \
      \FOREIGN KEY(epiShowID) REFERENCES TvShow(showID), \
      \FOREIGN KEY(epiSeasonID) REFERENCES Season(seasonID) \
      \)"
      []
    commit conn
    print "Table Episode has been created"
    return conn

tvShowToSqlValues :: TvShow -> [SqlValue]
tvShowToSqlValues tvShow = [
  toSql $ showID tvShow,
  toSql $ showName tvShow,
  toSql $ language tvShow,
  toSql $ runtime tvShow,
  toSql $ premiered tvShow,
  toSql $ url tvShow,
  toSql $ showSummary tvShow
 ]

prepareInsertTvShowStmt :: Connection -> IO Statement
prepareInsertTvShowStmt conn = prepare conn "INSERT INTO TvShow VALUES (?,?,?,?,?,?,?)"

saveTvShow :: TvShow -> Connection -> IO ()
saveTvShow tvShow conn = do
  stmt <- prepareInsertTvShowStmt conn
  execute stmt (tvShowToSqlValues tvShow)
  commit conn