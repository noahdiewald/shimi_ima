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

main = parseCSVFromFile "./VTI_CleanForImport.csv" >>= \retval ->
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
        , uriRegName = "144.92.237.11"
--        , uriRegName = "127.0.0.1"
        , uriPort = ":5984"
        }
--      , uriPath = "/project-4d915decf693d51ab06a2f109210a0b8"
      , uriPath = "/project-d153156fa3ed39fc1d4a21d57ce9c6d5"
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
  , cheadword_source
  , cheadword_speaker
  , cpos
  , cunderlying_representation
  , cdefinition1
  , cdefinition1_source
  , cdefinition2
  , cdefinition2_source
  , cdefinition3
  , cdefinition3_source
  , cdefinition4
  , cdefinition4_source
  , cdefinition5
  , cdefinition5_source
  , cvariant_form1
  , cvariant_form1_speaker
  , cvariant_form1_source
  , cinflected_form1
  , cinflected_form1_type
  , cinflected_form1_speaker
  , cinflected_form1_source
  , cinflected_form2
  , cinflected_form2_type
  , cinflected_form2_speaker
  , cinflected_form2_source
  , cinflected_form3
  , cinflected_form3_type
  , cinflected_form3_speaker
  , cinflected_form3_source
  , cexample1
  , cexample_definition1
  , cexample_speaker1
  , cexample_source1
  , cexample2
  , cexample_definition2
  , cexample_speaker2
  , cexample_source2
  , ccross_reference
  , cnotes
  , csource
  , _
  , crecord_id
  , _
  , _
  , cjdn_id
  , cchecked_with_elders
  ] = render . pp_value . showJSON $ makeDocument Entry
  { hw = Headword
    { headword = cheadword
    , lexical_category = cpos
    , elders_checked = Nothing
    , elders_checked_date = Nothing
    , elders_checked_initials = Nothing
    , hw_speaker = maybeList cheadword_speaker
    , hw_dialect = Nothing 
    , hw_source = sourceList csource
    , underlying_representation = maybeString cunderlying_representation
    , topic = Nothing
    , hw_notes = Just (makeNotes crecord_id cnotes cjdn_id "")
    , cross_reference = maybeString ccross_reference
    , unpublish = False
    , codes = Just ["lw_vti", "quarantine"]
    }
  , hwp = HWProps Nothing Nothing Nothing Nothing
  , hws = HWSound Nothing Nothing Nothing Nothing Nothing
  , defs = filterDefinitions [
    (cdefinition1, csource)
    , (cdefinition2, csource)
    , (cdefinition3, csource)
    , (cdefinition4, csource)
    , (cdefinition5, csource)
    ] 
  , kws = Keywords []
  , ex = filterExamples [
    (cexample1, cexample_definition1, cexample_speaker1, csource)
    , (cexample2, cexample_definition2, cexample_speaker2, csource)
    ]
  , ifs = filterInflectedForms [
    (cinflected_form1, cinflected_form1_type, cinflected_form1_speaker, csource)
    , (cinflected_form2, cinflected_form2_type, cinflected_form2_speaker, csource)
    , (cinflected_form3, cinflected_form3_type, cinflected_form3_speaker, csource)
    ]
  , udefs = UnusedDefinitions []
  , ofs = OtherForms []
  , usg = Usage Nothing Nothing Nothing Nothing
  , vfs = filterVariantForms [
    (cvariant_form1, "", cvariant_form1_speaker, csource)
    ]
  , pic = Picture Nothing Nothing Nothing Nothing Nothing Nothing
  , cp = CheckPoints (maybeBool cchecked_with_elders) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  }
processRecord _ = ""

makeNotes :: String -> String -> String -> String -> String
makeNotes r n j g = f_r r ++ f_n n ++ f_j j ++ f_n g
  where 
    f_r [] = []
    f_r s = "Original Record ID: " ++ s
    f_j [] = []
    f_j j = "\n\nJDN Number: " ++ j
    f_n [] = []
    f_n s = "\n\n" ++ n
    
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

sourceList :: String -> Maybe [String]
sourceList [] = Just ["LBW"]
sourceList str = Just [str, "LBW"]

firstNullOf4 :: (String, String, String, String) -> Bool
firstNullOf4 (x, _, _, _) = null x

firstNullOf2 :: (String, String) -> Bool
firstNullOf2 (x, _) = null x

filterDefinitions :: [(String, String)] -> Definitions
filterDefinitions xs =  Definitions [ makeDefinition x | x <- xs, (not . firstNullOf2) x]

filterExamples :: [(String, String, String, String)] -> Examples
filterExamples xs =  Examples [ makeExample x | x <- xs, (not . firstNullOf4) x]

filterInflectedForms :: [(String, String, String, String)] -> InflectedForms
filterInflectedForms xs =  InflectedForms [ makeInflectedForm x | x <- xs, (not . firstNullOf4) x]

filterVariantForms :: [(String, String, String, String)] -> VariantForms
filterVariantForms xs =  VariantForms [ makeVariantForm x | x <- xs, (not . firstNullOf4) x]

makeExample :: (String, String, String, String) -> Example
makeExample (ex, df, sp, _) = Example
  { example = maybeString ex
  , translation = maybeString df
  , example_sound = Nothing
  , ex_speaker = maybeList sp
  , ex_recording = Nothing
  , ex_timestamp = Nothing
  , example_date = Nothing
  , ex_notes = Nothing
  }

makeInflectedForm :: (String, String, String, String) -> InflectedForm
makeInflectedForm (ifl, ift, ifs, ifsou) = InflectedForm
  { inflected_form = maybeString ifl
  , inflection_type = maybeString ift
  , if_definition = Nothing
  , if_speaker = maybeList ifs
  , if_source = sourceList ifsou
  , if_elders_checked = Nothing
  , if_elders_checked_date = Nothing
  , if_elders_checked_initials = Nothing
  , if_sound_sample = Nothing
  , if_recording = Nothing
  , if_timestamp = Nothing
  , if_recording_date = Nothing
  , if_notes = Nothing
  }

makeDefinition :: (String, String) -> Definition
makeDefinition (df, dfs) = Definition
  { def_order = Nothing
  , def_definition = maybeString df
  , def_speaker = Nothing
  , def_source = sourceList dfs
  , def_notes = Nothing
  , def_elders_checked = Nothing
  , def_elders_checked_date = Nothing
  , def_elders_checked_initials = Nothing
  }

makeVariantForm :: (String, String, String, String) -> VariantForm
makeVariantForm (vfm, _, vfs, ifsou) = VariantForm
  { variant_form = maybeString vfm
  , vf_speaker = maybeList vfs
  , vf_dialect = Nothing
  , vf_source =  sourceList ifsou
  , vf_recording = Nothing
  , vf_timestamp = Nothing
  , vf_notes = Nothing
  }

