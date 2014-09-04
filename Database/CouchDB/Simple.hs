module Database.CouchDB.Simple where

import Network.HTTP
import Network.URI
import Control.Applicative

type DocId = String
type DocBody = String
type View = String

moduleName :: String
moduleName = "Database.CouchDB.Simple"

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

reqCT :: Request String -> IO String
reqCT r = let r' = insertHeader HdrContentType contentType r
          in simpleHTTP r' >>= getResponseBody

reqWithBody :: URI -> RequestMethod -> DocBody -> IO String
reqWithBody u m b = req $ setRequestBody (mkRequest m u) (contentType, b)

reqWithPath :: URI -> RequestMethod -> String -> IO String
reqWithPath u m s = req $ mkRequest m $ appendToPath u s

reqCTWithPath :: URI -> RequestMethod -> String -> IO String
reqCTWithPath u m s = reqCT $ mkRequest m $ appendToPath u s


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

-- | Accessing the root of a CouchDB instance returns meta information about the instance. The response is a JSON structure containing information about the server, including a welcome message and the version of the server.
getServerInfo :: Server -> IO String
getServerInfo = req . mkRequest GET


-- | List of running tasks, including the task type, name, status and process ID. The result is a JSON array of the currently running tasks, with each task being described with a single object. Depending on operation type set of response object fields might be different.
activeTasks :: Server -> IO String
activeTasks s = reqWithPath s GET "/_active_tasks" 

-- | Returns a list of all the databases in the CouchDB instance.
getAllDBs :: Server -> IO String
getAllDBs s = reqWithPath s GET "/_all_dbs"


-- | Returns a list of all database events in the CouchDB instance.
getUpdates :: Server -> IO String
getUpdates s = reqWithPath s GET "/_db_updates"

-- | Gets the CouchDB log, equivalent to accessing the local log file of the corresponding CouchDB instance.
getLog :: Server -> IO String
getLog s = reqWithPath s GET "/_log"

-- TODO: replicate

-- TODO: restart

{- TODO: stats
-- | The _stats resource returns a JSON object containing the statistics for the running server. The object is structured with top-level sections collating the statistics for a range of entries, with each individual statistic being easily identified, and the content of each statistic is self-describing
getStats :: Server -> IO String
getStats s = reqWithPath s GET "/_stats"
-}

-- | Accesses the built-in Futon administration interface for CouchDB.
utils :: Server -> IO String
utils s = reqWithPath s GET "/_utils/"

-- | Requests one or more Universally Unique Identifiers (UUIDs) from the CouchDB instance. The response is a JSON object providing a list of UUIDs.
uuids :: Server -> IO String
uuids s = reqWithPath s GET "/_uuids/"
-- ask for a list of uuids instead
--uuids :: Server -> Int -> IO [String]

-- | Request a single UUID.
uuid :: Server -> IO String
uuid u = do 
  r <- reqWithPath u GET "/_uuids"
  return $ take 32 $ drop 11 r

-- | Binary content for the favicon.ico site icon.
favicon :: Server -> IO String
favicon s = reqWithPath s GET "/_uuids/"



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

putDoc :: DB -> DocBody -> IO String 
putDoc d body = do
  u <- uuid $ rmPath d
  putDocWithId d u body  


{-

let mydb = Data.Maybe.fromJust $ db "http://127.0.0.1:5984/test"
let mysrv = Data.Maybe.fromJust $ server "http://127.0.0.1:5984"

-}

