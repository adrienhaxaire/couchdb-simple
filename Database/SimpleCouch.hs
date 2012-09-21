module Database.SimpleCouch where

import Network.HTTP
import Network.URI
import Control.Applicative

type DocId = String
type DocBody = String

moduleName :: String
moduleName = "Database.SimpleCouch"

contentType :: String
contentType = "application/json"

-- local helpers
rmQuery :: URI -> URI
rmQuery u = u {uriQuery = ""}

rmFragment :: URI -> URI
rmFragment u = u {uriFragment = ""}

rmQF :: URI -> URI
rmQF = rmFragment . rmQuery

rmPath :: URI -> URI
rmPath u = u {uriPath = ""}

appendToPath :: URI -> String -> URI
appendToPath u s = u {uriPath = uriPath u ++ s}

req :: Request String -> IO String
req r = simpleHTTP r >>= getResponseBody

reqWithBody :: URI -> RequestMethod -> DocBody -> IO String
reqWithBody u m b = req $ setRequestBody (mkRequest m u) (contentType, b)

reqWithPath :: URI -> RequestMethod -> String -> IO String
reqWithPath u m s = req $ mkRequest m $ appendToPath u s

-- 
type DB = URI

db :: String -> Maybe DB
db urlString = let between = takeWhile (/= '/') . drop 1
                   trimPath y = y {uriPath = '/' : between (uriPath y)}
               in trimPath <$> rmQF <$> parseAbsoluteURI urlString

type Server = URI

server :: String -> Maybe Server 
server s = rmPath <$> db s

uuid :: Server -> IO String
uuid u = do 
  r <- reqWithPath u GET "/_uuids"
  return $ take 32 $ drop 11 r

-- ask for a list of uuids
--uuids :: Server -> Int -> IO [String]

putDB :: DB -> IO String
putDB = req . mkRequest PUT

deleteDB :: DB -> IO String
deleteDB = req . mkRequest DELETE

getDBInfo :: DB -> IO String
getDBInfo = req . mkRequest GET

getDBChanges :: DB -> IO String
getDBChanges d = reqWithPath d GET "/_changes" 

compactDB :: DB -> IO String
compactDB d = reqWithPath d POST "/_compact"

putDocWithId :: DB -> DocId -> DocBody -> IO String 
putDocWithId d docId = reqWithBody (appendToPath d ('/' : docId)) PUT

putDoc :: DB -> DocBody -> IO String 
putDoc d body = do
  u <- uuid $ rmPath d
  putDocWithId d u body  



{-

--compactDBViews
--cleanupDBViews


url = "http://localhost:5984/test/fhad8y7"

-}


