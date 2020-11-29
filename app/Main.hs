module Main where

import HTTP
import Parser
import Database

main :: IO ()
main = do
  let url = "http://api.tvmaze.com/shows/1"
  print "Downloading..."
  json <- download url
  print "Parsing..."
  case (parseTvShow json) of
    Left err -> print err
    Right show -> do
      print "Saving on DB.."
      conn <- initialiseDB
      print show
      saveTvShow show conn
      print "Done!"
