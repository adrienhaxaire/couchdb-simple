module Database.SimpleCouch where

import Network.HTTP hiding (host)
import Network.Stream
import Network.URI
import Text.ParserCombinators.Parsec 
import qualified Data.List as L
import Data.Maybe

type Host = String
type DB = String

moduleName :: String
moduleName = "Database.SimpleCouch"

contentType :: String
contentType = "application/json"

validToken :: Parser Char
validToken = letter <|> digit <|> oneOf ":@-"

sepBySlashes :: Parser [String]
sepBySlashes = (many validToken) `sepBy1` (char '/')

valid :: String -> Int -> Maybe String
valid s n = if (not $ isURI s) then Nothing
            else case parse sepBySlashes "" s of
                   Left _ -> Nothing
                   Right v -> if (length v < n) then Nothing
                              else Just $ concat $ L.intersperse "/" $ take n v

-- | Returns an empty string if error.
host :: String -> Maybe Host
host s = valid s 3 

db :: String -> Maybe DB
db s = valid s 4 


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

putDocWithId :: DB 
             -> String -- ^ the doc ID
             -> String -- ^ the body of the doc
             -> IO String 
putDocWithId d docId body = 
    case db d of
      Nothing -> error $ moduleName ++ ".putDoc: invalid database: " ++ d
      Just v -> do 
                req $ putRequestWithBody (v ++ "/" ++ docId) body

putDoc :: DB 
       -> String -- ^ the body of the doc
       -> IO String 
putDoc d body = do u <- uuid (fromMaybe "" $ host d) -- fails below if wrong DB
                   putDocWithId d u body 

-- url = "http://localhost:5984/test/fhad8y7"

-- needs auth
--compactDB :: String -> DBName -> IO String
--compactDB urlString dbname = 
--    simpleHTTP $ postRequest (urlString ++ "/" ++ dbname ++ "/_compact")
--compactDBViews
--cleanupDBViews
