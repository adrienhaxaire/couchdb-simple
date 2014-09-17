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

-- | Server smart constructor, needs IO operation to check that it is found
mkServer :: Hostname -> Port -> IO (Maybe Server)
mkServer h p = withSocketsDo $ do
                 res <- try $ getAddrInfo (Just defaultHints) (Just h) (Just $ show p) 
                     :: IO (Either SomeException [AddrInfo])
                 case res of
                    Left _ -> return Nothing
                    Right infos -> let s = Server h p (head infos)
                                   in return $ Just s

-- | Creates maybe a Server at 127.0.0.1:5984
mkLocalServer :: IO (Maybe Server)
mkLocalServer = mkServer "127.0.0.1" 5984
  

data Database = Database { server :: Server
                         , name :: String}
                deriving Show

-- | Database smart constructor, needs IO operation to check that it is found
mkDatabase :: Server -> String -> IO (Maybe Database)
mkDatabase s n = do 
  rsp <- get s n
  return
    (if C.length (snd $ C.breakSubstring (C.pack n) (body rsp)) == 0
       then Nothing else Just (Database s n))

-- | Creates maybe a Database on the local server
mkLocalDatabase :: String -- ^ name of the database
                -> IO (Maybe Database)
mkLocalDatabase name = do
  maybeServer <- mkLocalServer
  case maybeServer of
    Nothing -> return Nothing
    Just srv -> mkDatabase srv name


contentType :: String -> String
contentType s = s ++ "Content-Type: application/json\r\n"

getRequest :: Server -> String -> C.ByteString
getRequest s loc = C.pack $ concat ["GET /" ++ loc ++ " HTTP/1.1\r\n"
                                   , "Host: " ++ hostname s ++ "\r\n"
                                   ,"Connection: close\r\n"
                                   , "\r\n"]

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
      go :: [C.ByteString] -> IO C.ByteString
      go xs = do
        x <- recv sock 1024 
        case C.length x of
          0 -> return $ C.concat xs
          _ -> go (xs ++ [x])


----------------------------------------------------------------------
-- Server methods
----------------------------------------------------------------------

-- | Accessing the root of a CouchDB instance returns meta information about the
-- instance. The response is a JSON structure containing information about the
-- server, including a welcome message and the version of the server.
serverInfo :: Server -> IO Response
serverInfo s = get s ""

-- | List of running tasks, including the task type, name, status and process
-- ID. The result is a JSON array of the currently running tasks, with each task
-- being described with a single object. Depending on operation type set of
-- response object fields might be different.
activeTasks :: Server -> IO Response
activeTasks s = get s "_active_tasks" 

-- | Returns a list of all the databases in the CouchDB instance.
allDBs :: Server -> IO Response
allDBs s = get s "_all_dbs"

-- | Returns a list of all database events in the CouchDB instance.
dbUpdates :: Server -> IO Response
dbUpdates s = get s "_db_updates"

-- | Gets the CouchDB log, equivalent to accessing the local log file of the
-- corresponding CouchDB instance.
serverLog :: Server -> IO Response
serverLog s = get s "_log"


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

-- | Accesses the built-in Futon administration interface for CouchDB. Supposed to be accessed through a browser.
getUtils :: Server -> IO Response
getUtils s = get s "_utils/"

{-
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
putDB :: Database -> IO String
putDB = req . mkRequest PUT

deleteDB :: Database -> IO String
deleteDB = req . mkRequest DELETE

getDBInfo :: Database -> IO String
getDBInfo = req . mkRequest GET

getDBChanges :: Database -> IO String
getDBChanges d = reqWithPath d GET "/_changes" 

compactDB :: Database -> IO String
compactDB d = reqCTWithPath d POST "/_compact"
  
compactView :: Database -> View -> IO String
compactView d v = reqCTWithPath d POST ("/_compact/" ++ v)

cleanupViews :: Database -> IO String
cleanupViews d = reqCTWithPath d POST "/_view_cleanup"

execTempView :: Database -> DocBody -> IO String
execTempView d b = reqWithBody (appendToPath d "/_temp_view") POST b

ensureFullCommit :: Database -> IO String
ensureFullCommit d = reqCTWithPath d POST "/_ensure_full_commit"

bulkDocs :: Database -> DocBody -> IO String
bulkDocs d b = reqWithBody (appendToPath d "/_bulk_docs") POST b

purgeDB :: Database -> IO String
purgeDB d = reqCTWithPath d POST "/_purge"

-- | Returns a built-in view of all documents in this database
getAllDocs :: Database -> IO String
getAllDocs d = reqWithPath d GET "/_all_docs"

getAllDocsIncluded :: Database -> DocBody -> IO String
getAllDocsIncluded d b = 
    reqWithBody (appendToPath d "/_all_docs?include_docs=true") POST b

getAllDocsBetweenKeys :: Database -> String -> String -> IO String
getAllDocsBetweenKeys d start end = do
    let p = "/_all_docs?include_docs=true&startkey=%22" ++ start ++ "%22&startkey=%22" ++ end ++ "%22"
    print p
    reqWithPath d GET p


----------------------------------------------------------------------
-- Database document methods
----------------------------------------------------------------------
putDocWithId :: Database -> DocId -> DocBody -> IO String 
putDocWithId d docId = reqWithBody (appendToPath d ('/' : docId)) PUT


{-
putDoc :: Database -> DocBody -> IO String 
putDoc d body = do
  u <- getUuid $ rmPath d
  putDocWithId d u body  
-}

-}

