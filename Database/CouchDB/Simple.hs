{-# LANGUAGE DeriveGeneric #-}
module Database.CouchDB.Simple where

import Control.Monad
import System.IO
import Control.Exception
import Data.Maybe

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString 
import qualified Data.ByteString.Char8 as C

moduleName :: String
moduleName = "Database.CouchDB.Simple"

data Server = Server { hostname :: String
                     , port :: Int
                     , info :: AddrInfo}
              deriving Show

type Hostname = String
type Port = Int

-- | Server smart constructor, needs IO operation to check it is found
mkServer :: Hostname -> Port -> IO (Maybe Server)
mkServer h p = withSocketsDo $ do
                 res <- try $ getAddrInfo (Just defaultHints) (Just h) (Just $ show p) 
                     :: IO (Either SomeException [AddrInfo])
                 case res of
                    Left _ -> return Nothing
                    Right infos -> let s = Server h p (head infos)
                                   in return $ Just s


data DB = DB { server :: Server
             , name :: String}
          deriving Show

-- not so smart constructor, checks to come
mkDB :: Server -> String -> IO (Maybe DB)
mkDB s n = do 
  rsp <- get s n
  if (C.length $ snd $ C.breakSubstring (C.pack n) (body rsp)) == 0
  then return Nothing
  else return $ Just (DB s n)


--localDB :: String -> Maybe DB
--localDB = Just . DB (Server "127.0.0.1" "5984")

contentType :: String -> String
contentType s = s ++ "Content-Type: application/json\r\n"

getRequest :: Server -> String -> C.ByteString
getRequest s loc = C.pack $ concat ["GET /" ++ loc ++ " HTTP/1.1\r\n"
                                   , "Host: " ++ hostname s ++ "\r\n"
                                   ,"Connection: close\r\n"
                                   , "\r\n\r\n"]

type Header = C.ByteString
type Body = C.ByteString

data Response = Response {header :: Header
                         , body :: Body}
              deriving Show

get :: Server -> String -> IO Response
get s@(Server h p i) loc = withSocketsDo $
    do sock <- socket (addrFamily i) Stream defaultProtocol
       connect sock (addrAddress i)
       sendAll sock $ getRequest s loc
       received <- recvAll sock
       close sock
       let (hdr, bdy) = C.breakSubstring (C.pack "\r\n\r\n") received 
       return $ Response hdr (C.drop 4 bdy)

recvAll :: Socket -> IO C.ByteString
recvAll sock = go []
    where
      go xs = do
        x <- recv sock 1024 
        if C.length x == 0
        then return $ C.concat xs
        else go (xs ++ [x])



{-


--
type DB = URI

-- | Returns a DB if its URL is valid
db :: String -> Maybe DB
db urlString = let between = takeWhile (/= '/') . drop 1
                   trimPath y = y {uriPath = '/' : between (uriPath y)}
               in trimPath <$> rmQF <$> parseAbsoluteURI urlString

type Server = URI

-- | Returns a Server if its URL is valid
server :: String -> Maybe Server 
server s = rmPath <$> db s


----------------------------------------------------------------------
-- Server methods
----------------------------------------------------------------------

-- | Accessing the root of a CouchDB instance returns meta information about the
-- instance. The response is a JSON structure containing information about the
-- server, including a welcome message and the version of the server.
getServerInfo :: Server -> IO String
getServerInfo = req . mkRequest GET


-- | List of running tasks, including the task type, name, status and process
-- ID. The result is a JSON array of the currently running tasks, with each task
-- being described with a single object. Depending on operation type set of
-- response object fields might be different.
activeTasks :: Server -> IO String
activeTasks s = reqWithPath s GET "/_active_tasks" 

-- | Returns a list of all the databases in the CouchDB instance.
getAllDBs :: Server -> IO String
getAllDBs s = reqWithPath s GET "/_all_dbs"


-- | Returns a list of all database events in the CouchDB instance.
getUpdates :: Server -> IO String
getUpdates s = reqWithPath s GET "/_db_updates"

-- | Gets the CouchDB log, equivalent to accessing the local log file of the
-- corresponding CouchDB instance.
getLog :: Server -> IO String
getLog s = reqWithPath s GET "/_log"

-- TODO: replicate

-- TODO: restart

{- TODO: stats
-- | The _stats resource returns a JSON object containing the statistics for the
-- running server. The object is structured with top-level sections collating
-- the statistics for a range of entries, with each individual statistic being
-- easily identified, and the content of each statistic is self-describing
getStats :: Server -> IO String
getStats s = reqWithPath s GET "/_stats"
-}

-- | Accesses the built-in Futon administration interface for CouchDB.
getUtils :: Server -> IO String
getUtils s = reqWithPath s GET "/_utils/"


data UUIDs = UUIDs {uuids :: [String]} deriving (Show, Generic)
instance ToJSON UUIDs
instance FromJSON UUIDs

-- | Requests one or more Universally Unique Identifiers (UUIDs) from the
-- CouchDB instance. The response is a JSON object providing a list of UUIDs.
getUuids :: Int -> Server -> IO [String]
getUuids n s = do
    rsp <- reqWithPath s GET ("/_uuids?count=" ++ show n)
    case decode $ ByteString.pack rsp of
      Just xs -> return $ uuids xs
      Nothing -> return []

-- | Request a single UUID.
getUuid :: Server -> IO String
getUuid s = do
  [rsp] <- getUuids 1 s
  return rsp


-- | Binary content for the favicon.ico site icon.
getFavicon :: Server -> IO String
getFavicon s = reqWithPath s GET "/favicon.ico"



----------------------------------------------------------------------
-- Database methods
----------------------------------------------------------------------
putDB :: DB -> IO String
putDB = req . mkRequest PUT

deleteDB :: DB -> IO String
deleteDB = req . mkRequest DELETE

getDBInfo :: DB -> IO String
getDBInfo = req . mkRequest GET

getDBChanges :: DB -> IO String
getDBChanges d = reqWithPath d GET "/_changes" 

compactDB :: DB -> IO String
compactDB d = reqCTWithPath d POST "/_compact"
  
compactView :: DB -> View -> IO String
compactView d v = reqCTWithPath d POST ("/_compact/" ++ v)

cleanupViews :: DB -> IO String
cleanupViews d = reqCTWithPath d POST "/_view_cleanup"

execTempView :: DB -> DocBody -> IO String
execTempView d b = reqWithBody (appendToPath d "/_temp_view") POST b

ensureFullCommit :: DB -> IO String
ensureFullCommit d = reqCTWithPath d POST "/_ensure_full_commit"

bulkDocs :: DB -> DocBody -> IO String
bulkDocs d b = reqWithBody (appendToPath d "/_bulk_docs") POST b

purgeDB :: DB -> IO String
purgeDB d = reqCTWithPath d POST "/_purge"

-- | Returns a built-in view of all documents in this database
getAllDocs :: DB -> IO String
getAllDocs d = reqWithPath d GET "/_all_docs"

getAllDocsIncluded :: DB -> DocBody -> IO String
getAllDocsIncluded d b = 
    reqWithBody (appendToPath d "/_all_docs?include_docs=true") POST b

getAllDocsBetweenKeys :: DB -> String -> String -> IO String
getAllDocsBetweenKeys d start end = do
    let p = "/_all_docs?include_docs=true&startkey=%22" ++ start ++ "%22&startkey=%22" ++ end ++ "%22"
    print p
    reqWithPath d GET p


----------------------------------------------------------------------
-- Database document methods
----------------------------------------------------------------------
putDocWithId :: DB -> DocId -> DocBody -> IO String 
putDocWithId d docId = reqWithBody (appendToPath d ('/' : docId)) PUT


{-
putDoc :: DB -> DocBody -> IO String 
putDoc d body = do
  u <- getUuid $ rmPath d
  putDocWithId d u body  
-}


{-

let mydb = Data.Maybe.fromJust $ db "http://127.0.0.1:5984/test"
let mysrv = Data.Maybe.fromJust $ server "http://127.0.0.1:5984"

-}

{-
"{\"couchdb\":\"Welcome\",
\"uuid\":\"e4f7ebb1496241537475c0b1451cf124\",
\"version\":\"1.6.0\",
\"vendor\":{\"version\":\"1.6.0\",
             \"name\":\"The Apache Software Foundation\"}}\n"

-}



data Vendor = Vendor { vendorVersion :: String
                     , vendorName :: String}
            deriving (Show, Generic)

instance ToJSON Vendor
instance FromJSON Vendor

data ServerInfo = ServerInfo { serverInfoWelcome :: String
                             , serverInfoUuid :: String
                             , serverInfoVersion :: String
                             , serverInfoVendor :: Vendor}
            deriving (Show, Generic)

instance ToJSON ServerInfo
instance FromJSON ServerInfo


-}
