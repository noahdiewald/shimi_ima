module Main where

import           Control.Applicative
import           Control.Monad.Trans
import           Data.Maybe
import           Text.CSV
import           Data.String.Utils
import           Text.JSON
import           Potawatomi
import           DictionaryDocument
import           Text.ParserCombinators.Parsec.Error
import           Network.HTTP
import           Network.Stream
import           Network.URI
import           Text.JSON.Pretty (pp_value)
import           Text.JSON.Pretty (render)
import qualified Data.ByteString.Char8 as B

main = parseCSVFromFile "./MISC_CleanForImport.csv" >>= \retval ->
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
        , uriRegName = "staging.ling.wisc.edu"
        , uriPort = ":5984"
        }
      , uriPath = "/project-1d629ffe875cb5f6593372c56d0d970d"
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
processRecord 
  [ cheadword
  , cheadword_speaker
  , cpos
  , cunderlying_representation
  , _
  , cdefinition1
  , cdefinition2
  , cdefinition3
  , cdefinition4
  , cdefinition5
  , cexample1
  , cexample_definition1
  , cexample_speaker1
  , cexample2
  , cexample_definition2
  , cexample_speaker2
  , cexample3
  , cexample_definition3
  , cexample_speaker3
  , ccross_reference
  , cnotes1
  , cnotes2
  , csource
  , _
  , crecord_id
  , _
  ] = render . pp_value . showJSON $ makeDocument Entry
  { hw = Headword
    { headword = cheadword
    , lexical_category = cpos
    , hw_speaker = maybeList cheadword_speaker
    , hw_dialect = Nothing 
    , hw_source = maybeList csource
    , underlying_representation = maybeString cunderlying_representation
    , topic = Nothing
    , hw_notes = Just (cnotes1 ++ "\n\n" ++ cnotes2 ++ "\n\nOriginal Record ID: " ++ crecord_id) 
    , cross_reference = maybeString ccross_reference
    , publish = False
    , codes = Just ["lw_misc"]
    }
  , hwp = HWProps Nothing Nothing Nothing Nothing
  , hws = HWSound Nothing Nothing Nothing Nothing Nothing
  , defs = filterDefinitions [
    cdefinition1
    , cdefinition2
    , cdefinition3
    , cdefinition4
    , cdefinition5
    ] 
  , kws = Keywords []
  , ex = filterExamples [
    (cexample1, cexample_definition1, cexample_speaker1)
    , (cexample2, cexample_definition2, cexample_speaker2)
    , (cexample3, cexample_definition3, cexample_speaker3)
    ]
  , ifs = InflectedForms []
  , udefs = UnusedDefinitions []
  , ofs = OtherForms []
  , usg = Usage Nothing Nothing Nothing Nothing
  , vfs = VariantForms []
  , pic = Picture Nothing Nothing Nothing Nothing Nothing Nothing
  , cp = CheckPoints Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  }
processRecord _ = ""

maybeString :: String -> Maybe String
maybeString [] = Nothing
maybeString str = Just str

maybeList :: String -> Maybe [String]
maybeList [] = Nothing
maybeList str = Just [str]

firstNullOf3 :: (String, String, String) -> Bool
firstNullOf3 (x, _, _) = null x

filterDefinitions :: String -> Definitions
filterDefinitions xs =  Definitions [ makeDefinition x | x <- xs, (not . null) x]

filterExamples :: [(String, String, String)] -> Examples
filterExamples xs =  Examples [ makeExample x | x <- xs, (not . firstNullOf3) x]

filterInflectedForms :: [(String, String, String)] -> InflectedForms
filterInflectedForms xs =  InflectedForms [ makeInflectedForm x | x <- xs, (not . firstNullOf3) x]

makeExample :: (String, String, String) -> Example
makeExample (ex, df, sp) = Example
  { example = maybeString ex
  , translation = maybeString df
  , example_sound = Nothing
  , ex_speaker = maybeList sp
  , ex_recording = Nothing
  , ex_timestamp = Nothing
  , example_date = Nothing
  , ex_notes = Nothing
  }

makeInflectedForm :: (String, String, String) -> InflectedForm
makeInflectedForm (ifl, ift, ifs) = InflectedForm
  { inflected_form = maybeString ifl
  , inflection_type = maybeString ift
  , if_definition = Nothing
  , if_speaker = maybeList ifs
  , if_elders_checked = Nothing
  , if_elders_checked_date = Nothing
  , if_elders_checked_initials = Nothing
  , if_sound_sample = Nothing
  , if_recording = Nothing
  , if_timestamp = Nothing
  , if_recording_date = Nothing
  , if_notes = Nothing
  }

makeDefinition :: String -> Definition
makeDefinition df = Definition
  { def_order = Nothing
  , def_definition = maybeString df
  , def_speaker = Nothing
  , def_source = Nothing
  , def_notes = Nothing
  , def_elders_checked = Nothing
  , def_elders_checked_date = Nothing
  , def_elders_checked_initials = Nothing
  }
