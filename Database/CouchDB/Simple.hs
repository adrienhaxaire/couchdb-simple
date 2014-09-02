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

db :: String -> Maybe DB
db urlString = let between = takeWhile (/= '/') . drop 1
                   trimPath y = y {uriPath = '/' : between (uriPath y)}
               in trimPath <$> rmQF <$> parseAbsoluteURI urlString

type Server = URI

server :: String -> Maybe Server 
server s = rmPath <$> db s


----------------------------------------------------------------------
-- Server level miscellaneous methods
----------------------------------------------------------------------
uuid :: Server -> IO String
uuid u = do 
  r <- reqWithPath u GET "/_uuids"
  return $ take 32 $ drop 11 r

-- ask for a list of uuids
--uuids :: Server -> Int -> IO [String]




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

