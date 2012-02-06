module ShortKeywords where

import Text.JSON
import Data.Typeable
import Data.Data
import Data.Time
import DictionaryDocument

data ShortKeyword = ShortKeyword { kw :: SKeyword } deriving (Show)

data SKeyword = SKeyword
              { skw :: String
              , notes :: Maybe String
              } deriving (Show)

instance DDocument ShortKeyword where
    makeDocument shortkeyword = 
        Document
        { doctype = "Keywords"
        , description = "Keywords used for English to Potawatomi lookup."
        , fieldsets = [ makeFieldset $ kw shortkeyword ]
        }

instance DFieldset SKeyword where
    makeFieldset skwRecord =
        SingleFieldset
        { s_identifier = "3220bb2bb5c6652da3728c4cb70a86a6"
        , s_collapse = False
        , s_name = "keyword"
        , s_label = "Keyword"
        , s_order = 5
        , s_fields =
            [ Field 
              { f_identifier = "d5331cbb4d62fe3d2899f142d90bbf87"
              , f_name = "value_"
              , f_label = "Keyword"
              , f_head = True
              , f_reversal = False
              , f_required = True
              , f_min = ""
              , f_max = ""
              , f_regex = ""
              , f_order = 5
              , f_index = 0
              , f_subcategory = "text"
              , f_value = showJSON $ skw skwRecord
              }
            , Field
              { f_identifier = "d5331cbb4d62fe3d2899f142d90bd4ab"
              , f_name = "notes"
              , f_label = "Notes"
              , f_head = False
              , f_reversal = False
              , f_required = False
              , f_min = ""
              , f_max = ""
              , f_regex = ""
              , f_order = 10
              , f_index = 0
              , f_subcategory = "textarea"
              , f_value = maybeBlank $ notes skwRecord
              }
            ]
        }