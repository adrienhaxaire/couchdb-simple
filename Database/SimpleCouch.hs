module Database.SimpleCouch where

import Network.HTTP
import Network.HTTP.Base
import Network.Stream (Result)
import Network.URI (parseURI, isURI)
import Text.ParserCombinators.Parsec 


type Server = String

type DB = String

contentType :: String
contentType = "application/json"

-- use isAllowedInURI

validToken :: Parser Char
validToken = letter <|> digit <|> oneOf ":@_-"

sepBySlashes :: Parser [String]
sepBySlashes = (many validToken) `sepBy1` (char '/')

validName :: String -> Int -> Bool
validName s n = case parse sepBySlashes "not needed" s of
                  Left _ -> False
                  Right v -> length v == n

-- | No trailing slashes allowed
validDBURL :: String -> Bool
validDBURL s = validName s 4


validServerURL :: String -> Bool
validServerURL s = validName s 3



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

uuid :: String -> IO String
uuid u = do 
  r <- req $ getRequest (u ++ "/_uuids")
  return $ take 32 $ drop 11 r

createDB :: DB -> IO String
createDB = req . putRequest

deleteDB :: DB -> IO String
deleteDB = req . deleteRequest

getDBInfo :: DB -> IO String
getDBInfo = req . getRequest

getDBChanges :: DB -> IO String
getDBChanges d = req $ getRequest (d ++ "/_changes") 

-- needs auth
--compactDB :: String -> DBName -> IO String
--compactDB urlString dbname = 
--    simpleHTTP $ postRequest (urlString ++ "/" ++ dbname ++ "/_compact")
--compactDBViews
--cleanupDBViews

createDoc :: DB -> String -> IO String 
createDoc d body = req $ putRequestWithBody d body


-- createDocWithId

