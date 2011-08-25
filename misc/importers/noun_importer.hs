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

main = parseCSVFromFile "./nouns.csv" >>= \retval ->
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
        , uriRegName = "localhost"
        , uriPort = ":5984"
        }
      , uriPath = "/project-b9ad37ea17a58d9be32160f3933b5731"
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
  , cdefinition
  , cplural
  , cplural_speaker
  , cposs
  , cposs_speaker
  , c3poss
  , c3poss_speaker
  , cdim
  , cdim_speaker
  , cpej
  , cpej_speaker
  , cloc
  , cloc_speaker
  , cobv
  , cobv_speaker
  , cvoc
  , cvoc_speaker
  , cexample1
  , cexample_definition1
  , cexample_speaker1
  , cexample2
  , cexample_definition2
  , cexample_speaker2
  , ccross_reference
  , cnotes
  , csource
  , ctape_time
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
    , hw_notes = Just (cnotes ++ "\n\nOriginal Record ID: " ++ crecord_id ++ "\n\nOriginal Tape Time: " ++ ctape_time) 
    , cross_reference = maybeString ccross_reference
    , publish = False
    , codes = Just ["lw_nouns"]
    }
  , hwp = HWProps Nothing Nothing Nothing Nothing
  , hws = HWSound Nothing Nothing Nothing Nothing Nothing
  , defs = Definitions 
    [Definition
      { def_order = Nothing
      , def_definition = maybeString cdefinition
      , def_speaker = Nothing
      , def_source = Nothing
      , def_notes = Nothing
      , def_elders_checked = Nothing
      , def_elders_checked_date = Nothing
      , def_elders_checked_initials = Nothing
      }
    ]
  , kws = Keywords []
  , ex = filterExamples [
    (cexample1, cexample_definition1, cexample_speaker1)
    , (cexample2, cexample_definition2, cexample_speaker2)
    ]
  , ifs = filterInflectedForms [
    (cplural, "plural", cplural_speaker)
    , (cposs, "poss", cposs_speaker)
    , (c3poss, "3.poss", c3poss_speaker)
    , (cpej, "pej", cpej_speaker)
    , (cdim, "dim", cdim_speaker)
    , (cloc, "loc", cloc_speaker)
    , (cobv, "obv", cobv_speaker)
    , (cvoc, "voc", cvoc_speaker)
    ]
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

