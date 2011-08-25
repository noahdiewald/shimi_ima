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

main = parseCSVFromFile "./VII_CleanForImport.csv" >>= \retval ->
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
  , cgeneral_notes
  , cdefinition
  , cinflected_form1
  , cinflected_form1_type
  , cinflected_form1_speaker
  , cinflected_form2
  , cinflected_form2_type
  , cinflected_form2_speaker
  , cinflected_form3
  , cinflected_form3_type
  , cinflected_form3_augment
  , cinflected_form3_speaker
  , cinflected_form4
  , cinflected_form4_type
  , cinflected_form4_speaker
  , cexample1
  , cexample_definition1
  , cexample_speaker1
  , cexample2
  , cexample_definition2
  , cexample_speaker2
  , ccross_reference
  , cnotes
  , csource
  , ctimestamp
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
    , hw_notes = Just (cgeneral_notes ++ "\n\n" ++ cnotes ++ "\n\nOriginal Record ID: " ++ crecord_id) 
    , cross_reference = maybeString ccross_reference
    , publish = False
    , codes = Just ["lw_vii"]
    }
  , hwp = HWProps (maybeBool cinflected_form3_augment) Nothing Nothing Nothing
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
  , ex = Examples
    [ Example
      { example = maybeString cexample1
      , translation = maybeString cexample_definition1
      , example_sound = Nothing
      , ex_speaker = maybeList cexample_speaker1
      , ex_recording = Nothing
      , ex_timestamp = Nothing
      , example_date = Nothing
      , ex_notes = Nothing
      }
    , Example
      { example = maybeString cexample2
      , translation = maybeString cexample_definition2
      , example_sound = Nothing
      , ex_speaker = maybeList cexample_speaker2
      , ex_recording = Nothing
      , ex_timestamp = Nothing
      , example_date = Nothing
      , ex_notes = Nothing
      }
    ]
  , ifs = InflectedForms 
    [ InflectedForm
      { inflected_form = maybeString cinflected_form1
      , inflection_type = maybeString cinflected_form1_type
      , if_definition = Nothing
      , if_speaker = maybeList cinflected_form1_speaker
      , if_elders_checked = Nothing
      , if_elders_checked_date = Nothing
      , if_elders_checked_initials = Nothing
      , if_sound_sample = Nothing
      , if_recording = Nothing
      , if_timestamp = Nothing
      , if_recording_date = Nothing
      , if_notes = Nothing
      }
    , InflectedForm
      { inflected_form = maybeString cinflected_form2
      , inflection_type = maybeString cinflected_form2_type
      , if_definition = Nothing
      , if_speaker = maybeList cinflected_form2_speaker
      , if_elders_checked = Nothing
      , if_elders_checked_date = Nothing
      , if_elders_checked_initials = Nothing
      , if_sound_sample = Nothing
      , if_recording = Nothing
      , if_timestamp = Nothing
      , if_recording_date = Nothing
      , if_notes = Nothing
      }
    , InflectedForm
      { inflected_form = maybeString cinflected_form3
      , inflection_type = maybeString cinflected_form3_type
      , if_definition = Nothing
      , if_speaker = maybeList cinflected_form3_speaker
      , if_elders_checked = Nothing
      , if_elders_checked_date = Nothing
      , if_elders_checked_initials = Nothing
      , if_sound_sample = Nothing
      , if_recording = Nothing
      , if_timestamp = Nothing
      , if_recording_date = Nothing
      , if_notes = Nothing
      } 
    , InflectedForm
      { inflected_form = maybeString cinflected_form4
      , inflection_type = maybeString cinflected_form4_type
      , if_definition = Nothing
      , if_speaker = maybeList cinflected_form4_speaker
      , if_elders_checked = Nothing
      , if_elders_checked_date = Nothing
      , if_elders_checked_initials = Nothing
      , if_sound_sample = Nothing
      , if_recording = Nothing
      , if_timestamp = Nothing
      , if_recording_date = Nothing
      , if_notes = Nothing
      } 
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

maybeBool :: String -> Maybe Bool
maybeBool "TRUE" = Just True
maybeBool "FALSE" = Just False
maybeBool _ = Nothing
