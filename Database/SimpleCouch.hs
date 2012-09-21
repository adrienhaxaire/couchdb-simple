module Database.SimpleCouch where

import Network.HTTP hiding (host)
import Network.Stream
import Network.URI
import Text.ParserCombinators.Parsec 
import qualified Data.List as L
import Data.Maybe
import Control.Applicative

type Host = URI
type DB = URI

type DocId = String
type DocBody = String

moduleName :: String
moduleName = "Database.SimpleCouch"

contentType :: String
contentType = "application/json"

db :: String -> Maybe DB
db urlString = let removeQF x = x {uriQuery = "", uriFragment = ""}
                   between = takeWhile (/= '/') . drop 1
                   trimPath y = y {uriPath = '/' : (between $ uriPath y)}
               in trimPath <$> removeQF <$> parseAbsoluteURI urlString

host :: String -> Maybe Host 
host s = (\u -> u {uriPath = ""}) <$> db s


url = "http://localhost:5984/test/fhad8y7"


{-


putRequest :: String -> Request String
putRequest urlString = 
    case parseURI urlString of
      Nothing -> error ("putRequest: Not a valid URL - " ++ urlString)
      Just u  -> mkRequest PUT u

putRequestWithBody :: String -> String -> Request String
putRequestWithBody urlString body = 
  case parseURI urlString of
    Nothing -> error ("putRequestWithBody: Not a valid URL - " ++ urlString)
    Just u  -> setRequestBody (mkRequest PUT u) (contentType, body)

deleteRequest :: String -> Request String
deleteRequest urlString = 
    case parseURI urlString of
      Nothing -> error ("deleteRequest: Not a valid URL - " ++ urlString)
      Just u  -> mkRequest DELETE u

req :: Request String -> IO String
req r = simpleHTTP r >>= getResponseBody >>= return

uuid :: Host -> IO String
uuid u = do 
  r <- req $ getRequest (u ++ "/_uuids")
  return $ take 32 $ drop 11 r

-- ask for a list of uuids
--uuids :: Host -> Int -> IO [String]
--uuids u = req $ getRequest (host u ++ "/_uuids")

putDB :: DB -> IO String
putDB = req . putRequest

deleteDB :: DB -> IO String
deleteDB = req . deleteRequest

getDBInfo :: DB -> IO String
getDBInfo = req . getRequest

getDBChanges :: DB -> IO String
getDBChanges d = req $ getRequest (d ++ "/_changes") 

putDocWithId :: DB -> DocId -> DocBody -> IO String 
putDocWithId d docId body = 
    case db d of
      Nothing -> error $ moduleName ++ ".putDoc: invalid database: " ++ d
      Just v -> do 
                req $ putRequestWithBody (v ++ "/" ++ docId) body

putDoc :: DB -> DocBody -> IO String 
putDoc d body = do u <- uuid (fromMaybe "" $ host d) 
                   putDocWithId d u body 


compactDB :: DB -> IO String
compactDB d = req $ postRequest (d ++ "/_compact")


--compactDBViews
--cleanupDBViews

-}