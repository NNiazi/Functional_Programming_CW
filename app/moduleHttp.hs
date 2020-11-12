module HTTP
    ( download
    ) where
import qualified  Data.ByteString.Lazy.Char8 as L8
import  Network.HTTP.Simple

type URL = String

download :: URL -> IO L8.ByteString
download url = do   
    request  <- parseRequest url
    response <-httpLbs request
    return $ getResponseBody response 
