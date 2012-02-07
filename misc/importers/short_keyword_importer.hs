module Main where

import           Control.Applicative
import           Control.Monad.Trans
import           Data.Maybe
import           Text.CSV
import           Data.String.Utils
import           Text.JSON
import           ShortKeywords
import           DictionaryDocument
import           Text.ParserCombinators.Parsec.Error
import           Network.HTTP
import           Network.Stream
import           Network.URI
import           Text.JSON.Pretty (pp_value)
import           Text.JSON.Pretty (render)
import qualified Data.ByteString.Char8 as B

main = parseCSVFromFile "./short_keywords.csv" >>= \retval ->
  convertToJson retval >>= \rjson -> sequence $ fmap sendRecord rjson
  where
    convertToJson (Left error) = return $ map messageString $ errorMessages error
    convertToJson (Right (r:rs)) = return $  reverse . tail . reverse $ map processRecord rs

sendRecord :: String -> IO (Either Network.Stream.ConnError (Response B.ByteString))
sendRecord str = putStrLn str >> simpleHTTP 
  Request 
    { rqURI = URI 
      { uriScheme = "http:"
      , uriAuthority = Just URIAuth 
        { uriUserInfo = "database:D1ctionary_Mak3r@"
        , uriRegName = "127.0.0.1"
        , uriPort = ":5984"
        }
      , uriPath = "/project-4d915decf693d51ab06a2f109210a0b8"
      , uriQuery = ""
      , uriFragment = ""
      }
    , rqMethod = POST
    , rqHeaders = 
      [ Header HdrContentType "application/json;charset=utf-8"
      , Header HdrContentLength (show $ B.length body)
      ]
    , rqBody = body
    }
    where body = B.pack str

processRecord :: Record -> String
processRecord [ cshkw ] =
    render . pp_value . showJSON $ makeDocument 
           ShortKeyword 
           { kw = SKeyword
                  { skw = cshkw
                  , notes = Just "Added by import from Menominee dictionary data."
                  }
           }
