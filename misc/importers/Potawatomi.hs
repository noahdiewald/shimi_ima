module Potawatomi where

import           Text.JSON
import           Data.Typeable
import           Data.Data
import           Data.Time
import           DictionaryDocument

data Entry = Entry 
  { hw :: Headword
  , hwp :: HWProps
  , hws :: HWSound
  , defs :: Definitions
  , kws :: Keywords
  , ex :: Examples
  , ifs :: InflectedForms
  , udefs :: UnusedDefinitions
  , ofs :: OtherForms
  , usg :: Usage
  , vfs :: VariantForms
  , pic :: Picture
  , cp :: CheckPoints
  } deriving (Show)

data Headword = Headword
  { headword :: String
  , lexical_category :: String
  , hw_speaker :: Maybe [String]
  , hw_dialect :: Maybe [String]
  , hw_source :: Maybe [String]
  , underlying_representation :: Maybe String
  , topic :: Maybe [String]
  , hw_notes :: Maybe String
  , cross_reference :: Maybe String
  , publish :: Bool
  , codes :: Maybe [String]
  } deriving (Show)

data HWProps = HWProps
  { augment :: OpenBool
  , short_prefix :: OpenBool
  , long_prefix :: OpenBool
  , multiple_3rd_person_forms :: OpenBool
  } deriving (Show)
  
data HWSound = HWSound
  { hws_sound_sample :: Maybe String
  , hws_speaker :: Maybe String
  , hws_recording :: Maybe String
  , hws_timestamp :: Maybe Integer
  , hws_sound_date :: Maybe Day
  } deriving (Show)
  
data Definition = Definition
  { def_order :: Maybe Integer
  , def_definition:: Maybe String
  , def_speaker :: Maybe String
  , def_source :: Maybe [String]
  , def_notes :: Maybe String
  , def_elders_checked :: OpenBool
  , def_elders_checked_date :: Maybe Day
  , def_elders_checked_initials :: Maybe String
  } deriving (Show)

data Keyword = Keyword
  { short :: Maybe String
  , long :: Maybe String
  } deriving (Show)
  
data Example = Example
  { example :: Maybe String
  , translation :: Maybe String
  , example_sound :: Maybe String
  , ex_speaker :: Maybe [String]
  , ex_recording :: Maybe String
  , ex_timestamp :: Maybe Integer
  , example_date :: Maybe Day
  , ex_notes :: Maybe String
  } deriving (Show)
  
data InflectedForm = InflectedForm
  { inflected_form :: Maybe String
  , inflection_type :: Maybe String
  , if_definition :: Maybe String
  , if_speaker :: Maybe [String]
  , if_elders_checked :: OpenBool
  , if_elders_checked_date :: Maybe Day
  , if_elders_checked_initials :: Maybe String
  , if_sound_sample :: Maybe String
  , if_recording :: Maybe String
  , if_timestamp :: Maybe Integer
  , if_recording_date :: Maybe Day
  , if_notes :: Maybe String
  } deriving (Show)

data UnusedDefinition = UnusedDefinition
  { unused_definition :: Maybe String
  , ud_source :: Maybe [String]
  , ud_notes :: Maybe String
  } deriving (Show)

data OtherForm = OtherForm
  { other_form :: Maybe String
  , form_type :: Maybe String
  , of_source :: Maybe [String]
  , of_sound_sample :: Maybe String
  , of_speaker :: Maybe [String]
  , of_recording :: Maybe String
  , of_timestamp :: Maybe Integer
  , of_notes :: Maybe String
  } deriving (Show)

data Usage = Usage
  { usage_notes :: Maybe String
  , cultural_notes :: Maybe String
  , restricted :: OpenBool
  , restricted_note :: Maybe String
  } deriving (Show)
  
data VariantForm = VariantForm
  { variant_form :: Maybe String
  , vf_speaker :: Maybe [String]
  , vf_dialect :: Maybe String
  , vf_source :: Maybe [String]
  , vf_recording :: Maybe String
  , vf_timestamp :: Maybe Integer
  , vf_notes :: Maybe String
  } deriving (Show)

data Picture = Picture
  { picture :: Maybe String
  , content :: Maybe String
  , image_source :: Maybe String
  , copyright :: Maybe String
  , image_date :: Maybe Day
  , p_notes :: Maybe String
  } deriving (Show)
  
data CheckPoints = CheckPoints
  { cp_elders_checked :: OpenBool
  , cp_elders_checked_date :: Maybe Day
  , cp_elders_checked_initials :: Maybe String
  , source_checked :: OpenBool
  , source_checked_date :: Maybe Day
  , source_checked_initials :: Maybe String
  , morphemic_analysis_checked :: OpenBool
  , morphemic_analysis_checked_date :: Maybe Day
  , morphemic_analysis_checked_initials :: Maybe String
  , keywords_checked :: OpenBool
  , keywords_checked_date :: Maybe Day
  , keywords_checked_initials :: Maybe String
  , final_check :: OpenBool
  , final_check_date :: Maybe Day
  , final_check_initials :: Maybe String
  } deriving (Show)

data Definitions = Definitions [Definition] deriving (Show)

data Keywords = Keywords [Keyword] deriving (Show)

data Examples = Examples [Example] deriving (Show)

data InflectedForms = InflectedForms [InflectedForm] deriving (Show)

data UnusedDefinitions = UnusedDefinitions [UnusedDefinition] deriving (Show)

data OtherForms = OtherForms [OtherForm] deriving (Show)

data VariantForms = VariantForms [VariantForm] deriving (Show)

instance JSON Day where
  showJSON = showJSON . showGregorian

instance DDocument Entry where
  makeDocument entry = Document
    { doctype = "Entry"
    , description = "A dictionary entry."
    , fieldsets = 
      [ makeFieldset $ hw entry
      , makeFieldset $ hwp entry
      , makeFieldset $ hws entry
      , makeFieldset $ defs entry
      , makeFieldset $ kws entry
      , makeFieldset $ ex entry
      , makeFieldset $ ifs entry
      , makeFieldset $ udefs entry
      , makeFieldset $ ofs entry
      , makeFieldset $ usg entry
      , makeFieldset $ vfs entry
      , makeFieldset $ pic entry
      , makeFieldset $ cp entry
      ]
    }
    
instance DFieldset Headword where
  makeFieldset hwRecord = SingleFieldset 
    { s_identifier = "d5331cbb4d62fe3d2899f142d9036de5"
    , s_collapse = False
    , s_name = "headword"
    , s_label = "Headword"
    , s_order = 0
    , s_fields =
      [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d9073f36"
        , f_name = "headword"
        , f_label = "Headword"
        , f_head = True
        , f_reversal = False
        , f_required = True
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 5
        , f_index = 0
        , f_subcategory = "text"
        , f_value = showJSON $ headword hwRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90746b7"
        , f_name = "lexical_category"
        , f_label = "Lexical Category"
        , f_head = True
        , f_reversal = False
        , f_required = True
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 10
        , f_index = 0
        , f_subcategory = "docselect"
        , f_value = showJSON $ lexical_category hwRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d907545b"
        , f_name = "speaker"
        , f_label = "Speaker"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 15
        , f_index = 0
        , f_subcategory = "docmultiselect"
        , f_value = maybeNull $ hw_speaker hwRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9075f5f"
        , f_name = "dialect"
        , f_label = "Dialects"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 20
        , f_index = 0
        , f_subcategory = "docmultiselect"
        , f_value = maybeNull $ hw_dialect hwRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9076ec8"
        , f_name = "source"
        , f_label = "Source"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 25
        , f_index = 0
        , f_subcategory = "docmultiselect"
        , f_value = maybeNull $ hw_source hwRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9077856"
        , f_name = "underlying_representation"
        , f_label = "Underlying Representation"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 30
        , f_index = 0
        , f_subcategory = "text"
        , f_value = maybeBlank $ underlying_representation hwRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9079484"
        , f_name = "topic"
        , f_label = "Topic"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 35
        , f_index = 0
        , f_subcategory = "docmultiselect"
        , f_value = maybeNull $ topic hwRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d907a24e"
        , f_name = "notes"
        , f_label = "General Notes on Form"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 40
        , f_index = 0
        , f_subcategory = "textarea"
        , f_value = maybeBlank $ hw_notes hwRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d907b61e"
        , f_name = "cross_reference"
        , f_label = "Cross Reference"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 45
        , f_index = 0
        , f_subcategory = "text"
        , f_value = maybeBlank $ cross_reference hwRecord
        }
      , Field { f_identifier = "8e72d27187618f284e01dc9c5404afde"
        , f_name = "publish"
        , f_label = "Publish"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 55
        , f_index = 0
        , f_subcategory = "boolean"
        , f_value = showJSON $ publish hwRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d907dd6f"
        , f_name = "codes"
        , f_label = "Codes"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 55
        , f_index = 0
        , f_subcategory = "docmultiselect"
        , f_value = maybeNull $ codes hwRecord
        }
      ]
    }

instance DFieldset HWProps where
  makeFieldset hwProps = SingleFieldset
    { s_identifier = "d5331cbb4d62fe3d2899f142d9038635"
    , s_collapse = True
    , s_name = "headword_properties"
    , s_label = "Headword Props"
    , s_order = 5
    , s_fields =
      [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d9066a85"
        , f_name = "augment"
        , f_label = "AI and II: Augment"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 5
        , f_index = 0
        , f_subcategory = "openboolean"
        , f_value = maybeNull $ augment hwProps
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9067769"
        , f_name = "short_prefix"
        , f_label = "AI, TA, TI: Short Prefix"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 10
        , f_index = 0
        , f_subcategory = "openboolean"
        , f_value = maybeNull $ short_prefix hwProps
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9068d67"
        , f_name = "long_prefix"
        , f_label = "AI, TA, TI: Long Prefix"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 15
        , f_index = 0
        , f_subcategory = "openboolean"
        , f_value = maybeNull $ long_prefix hwProps
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d906ae23"
        , f_name = "multiple_3rd_person_forms"
        , f_label = "AI: Multiple 3rd Person Forms"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 20
        , f_index = 0
        , f_subcategory = "openboolean"
        , f_value = maybeNull $ multiple_3rd_person_forms hwProps
        }
      ]
    }

instance DFieldset HWSound where
  makeFieldset hwSound = SingleFieldset
    { s_identifier = "d5331cbb4d62fe3d2899f142d9039132"
    , s_collapse = True
    , s_name = "headword_sound"
    , s_label = "Headword Sound"
    , s_order = 10
    , s_fields =
      [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d906be60"
        , f_name = "sound_sample"
        , f_label = "Sound Sample"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 5
        , f_index = 0
        , f_subcategory = "text"
        , f_value = maybeBlank $ hws_sound_sample hwSound
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d906c64d"
        , f_name = "speaker"
        , f_label = "Speaker"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 10
        , f_index = 0
        , f_subcategory = "docselect"
        , f_value = maybeBlank $ hws_speaker hwSound
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d906e0cd"
        , f_name = "recording"
        , f_label = "Recording"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 15
        , f_index = 0
        , f_subcategory = "text"
        , f_value = maybeBlank $ hws_recording hwSound
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9070d8"
        , f_name = "timestamp"
        , f_label = "Timestamp"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 20
        , f_index = 0
        , f_subcategory = "integer"
        , f_value = maybeBlank $ hws_timestamp hwSound
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d907211e"
        , f_name = "sound_date"
        , f_label = "Sound Date"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 25
        , f_index = 0
        , f_subcategory = "date"
        , f_value = maybeBlank $ hws_sound_date hwSound
        }
      ]
    }

instance DFieldset Definitions where
  makeFieldset (Definitions ds) = MultipleFieldset
    { m_identifier = "d5331cbb4d62fe3d2899f142d9039eed"
    , m_collapse = True
    , m_name = "definition"
    , m_label = "Definition"
    , m_order = 15
    , m_multifields = map makeMultiField ds
    }

instance DFieldset Keywords where
  makeFieldset (Keywords ks) = MultipleFieldset
    { m_identifier = "d5331cbb4d62fe3d2899f142d903af7b"
    , m_collapse = False
    , m_name = "keywords"
    , m_label = "Keywords"
    , m_order = 20
    , m_multifields = map makeMultiField ks
    }

instance DFieldset Examples where
  makeFieldset (Examples es) = MultipleFieldset
    { m_identifier = "d5331cbb4d62fe3d2899f142d903c781"
    , m_collapse = True
    , m_name = "examples"
    , m_label = "Examples"
    , m_order = 25
    , m_multifields = map makeMultiField es
    }

instance DFieldset InflectedForms where
  makeFieldset (InflectedForms ifs) = MultipleFieldset
    { m_identifier = "d5331cbb4d62fe3d2899f142d9040945"
    , m_collapse = True
    , m_name = "inflected_forms"
    , m_label = "Inflected Forms"
    , m_order = 30
    , m_multifields = map makeMultiField ifs
    }

instance DFieldset UnusedDefinitions where
  makeFieldset (UnusedDefinitions uds) = MultipleFieldset
    { m_identifier = "d5331cbb4d62fe3d2899f142d90427c2"
    , m_collapse = True
    , m_name = "unused_definitions"
    , m_label = "Unused Defs."
    , m_order = 35
    , m_multifields = map makeMultiField uds
    }


instance DFieldset OtherForms where
  makeFieldset (OtherForms ofs) = MultipleFieldset
    { m_identifier = "d5331cbb4d62fe3d2899f142d9043869"
    , m_collapse = True
    , m_name = "other_forms"
    , m_label = "Other Forms"
    , m_order = 40
    , m_multifields = map makeMultiField ofs
    }

instance DFieldset Usage where
  makeFieldset usage = SingleFieldset
    { s_identifier = "d5331cbb4d62fe3d2899f142d9044ed5"
    , s_collapse = True
    , s_name = "usage"
    , s_label = "Usage"
    , s_order = 45
    , s_fields =
      [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d90a66e6"
        , f_name = "usage_notes"
        , f_label = "Usage Notes"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 5
        , f_index = 0
        , f_subcategory = "textarea"
        , f_value = maybeBlank $ usage_notes usage
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90a7a36"
        , f_name = "cultural_notes"
        , f_label = "Cultural Notes"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 10
        , f_index = 0
        , f_subcategory = "textarea"
        , f_value = maybeBlank $ cultural_notes usage
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90a8d67"
        , f_name = "restricted"
        , f_label = "Restricted Content"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 15
        , f_index = 0
        , f_subcategory = "openboolean"
        , f_value = maybeNull $ restricted usage
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90a9f43"
        , f_name = "restricted_note"
        , f_label = "Restricted Note"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 20
        , f_index = 0
        , f_subcategory = "textarea"
        , f_value = maybeBlank $ restricted_note usage
        }
      ]
    }
      
instance DFieldset VariantForms where
  makeFieldset (VariantForms vfs) = MultipleFieldset
    { m_identifier = "8e72d27187618f284e01dc9c54007bdf"
    , m_collapse = True
    , m_name = "variant_forms"
    , m_label = "Variant Forms"
    , m_order = 50
    , m_multifields = map makeMultiField vfs
    }

instance DFieldset Picture where
  makeFieldset pictureRecord = SingleFieldset
    { s_identifier = "d5331cbb4d62fe3d2899f142d9045cce"
    , s_collapse = True
    , s_name = "picture"
    , s_label = "Picture"
    , s_order = 50
    , s_fields =
      [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d909d4a9"
        , f_name = "picture"
        , f_label = "Sound Picture"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 5
        , f_index = 0
        , f_subcategory = "text"
        , f_value = maybeBlank $ picture pictureRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d909e786"
        , f_name = "content"
        , f_label = "Content"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 10
        , f_index = 0
        , f_subcategory = "text"
        , f_value = maybeBlank $ content pictureRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d909f0e1"
        , f_name = "image_source"
        , f_label = "Source of Image"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 15
        , f_index = 0
        , f_subcategory = "text"
        , f_value = maybeBlank $ image_source pictureRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d909fde1"
        , f_name = "copyright"
        , f_label = "Copyright"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 20
        , f_index = 0
        , f_subcategory = "text"
        , f_value = maybeBlank $ copyright pictureRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90a1498"
        , f_name = "image_date"
        , f_label = "Image Date"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 25
        , f_index = 0
        , f_subcategory = "date"
        , f_value = maybeBlank $ image_date pictureRecord
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90a1a3e"
        , f_name = "notes"
        , f_label = "Notes on Image"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 30
        , f_index = 0
        , f_subcategory = "textarea"
        , f_value = maybeBlank $ p_notes pictureRecord
        }
      ]
    }

instance DFieldset CheckPoints where
  makeFieldset checkPoints = SingleFieldset
    { s_identifier = "d5331cbb4d62fe3d2899f142d904780e"
    , s_collapse = True
    , s_name = "check_points"
    , s_label = "Check Points"
    , s_order = 55
    , s_fields =
      [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d90486fd"
        , f_name = "elders_checked"
        , f_label = "Checked with Elders"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 20
        , f_index = 0
        , f_subcategory = "openboolean"
        , f_value = maybeNull $ cp_elders_checked checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9049aa2"
        , f_name = "elders_checked_date"
        , f_label = "Checked with Elders Date"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 25
        , f_index = 0
        , f_subcategory = "date"
        , f_value = maybeBlank $ cp_elders_checked_date checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d904ad22"
        , f_name = "elders_checked_initials"
        , f_label = "Checked with Elders Initials"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 30
        , f_index = 0
        , f_subcategory = "docselect"
        , f_value = maybeBlank $ cp_elders_checked_initials checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d904c3ae"
        , f_name = "source_checked"
        , f_label = "Source Checked"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 35
        , f_index = 0
        , f_subcategory = "openboolean"
        , f_value = maybeNull $ source_checked checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d904da9d"
        , f_name = "source_checked_date"
        , f_label = "Source Checked Date"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 40
        , f_index = 0
        , f_subcategory = "date"
        , f_value = maybeBlank $ source_checked_date checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d904dda5"
        , f_name = "source_checked_initials"
        , f_label = "Source Checked Initials"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 45
        , f_index = 0
        , f_subcategory = "docselect"
        , f_value = maybeBlank $ source_checked_initials checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d904ec8d"
        , f_name = "morphemic_analysis_checked"
        , f_label = "Morphemic Analysis Checked"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 50
        , f_index = 0
        , f_subcategory = "openboolean"
        , f_value = maybeNull $ morphemic_analysis_checked checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d904fb78"
        , f_name = "morphemic_analysis_checked_date"
        , f_label = "Morphemic Analysis Checked Date"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 55
        , f_index = 0
        , f_subcategory = "date"
        , f_value = maybeBlank $ morphemic_analysis_checked_date checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90511c9"
        , f_name = "morphemic_analysis_checked_initials"
        , f_label = "Morphemic Analysis Checked Initials"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 60
        , f_index = 0
        , f_subcategory = "docselect"
        , f_value = maybeBlank $ morphemic_analysis_checked_initials checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9052acc"
        , f_name = "keywords_checked"
        , f_label = "Keywords Checked"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 65
        , f_index = 0
        , f_subcategory = "openboolean"
        , f_value = maybeNull $ keywords_checked checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90535b2"
        , f_name = "keywords_checked_date"
        , f_label = "Keywords Checked Date"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 70
        , f_index = 0
        , f_subcategory = "date"
        , f_value = maybeBlank $ keywords_checked_date checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d905484d"
        , f_name = "keywords_checked_initials"
        , f_label = "Keywords Checked Initials"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 75
        , f_index = 0
        , f_subcategory = "docselect"
        , f_value = maybeBlank $ keywords_checked_initials checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90555dc"
        , f_name = "final_check"
        , f_label = "Final Check"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 80
        , f_index = 0
        , f_subcategory = "openboolean"
        , f_value = maybeNull $ final_check checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9055de0"
        , f_name = "final_check_date"
        , f_label = "Final Check Date"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 85
        , f_index = 0
        , f_subcategory = "date"
        , f_value = maybeBlank $ final_check_date checkPoints
        }
      , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9056a72"
        , f_name = "final_check_initials"
        , f_label = "Final Check Initials"
        , f_head = False
        , f_reversal = False
        , f_required = False
        , f_min = ""
        , f_max = ""
        , f_regex = ""
        , f_order = 90
        , f_index = 0
        , f_subcategory = "docselect"
        , f_value = maybeBlank $ final_check_initials checkPoints
        }
      ]
    }

instance DMultiField Definition where
  makeMultiField definitionRecord = MultiField
    [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d90581d7"
      , f_name = "order"
      , f_label = "Order"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 5
      , f_index = 0
      , f_subcategory = "integer"
      , f_value = maybeBlank $ def_order definitionRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9058ce0"
      , f_name = "definition"
      , f_label = "Definition"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 10
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ def_definition definitionRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90592ee"
      , f_name = "speaker"
      , f_label = "Speaker"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 15
      , f_index = 0
      , f_subcategory = "docmultiselect"
      , f_value = maybeNull $ def_speaker definitionRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d905a4d7"
      , f_name = "source"
      , f_label = "Source"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 20
      , f_index = 0
      , f_subcategory = "docmultiselect"
      , f_value = maybeNull $ def_source definitionRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d905b08e"
      , f_name = "notes"
      , f_label = "Notes"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 25
      , f_index = 0
      , f_subcategory = "textarea"
      , f_value = maybeBlank $ def_notes definitionRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d905c9e1"
      , f_name = "elders_checked"
      , f_label = "Checked with Elders"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 30
      , f_index = 0
      , f_subcategory = "openboolean"
      , f_value = maybeNull $ def_elders_checked definitionRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d905dafa"
      , f_name = "elders_checked_date"
      , f_label = "Checked with Elders Date"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 35
      , f_index = 0
      , f_subcategory = "date"
      , f_value = maybeBlank $ def_elders_checked_date definitionRecord
      }
    , Field { f_identifier = "8e72d27187618f284e01dc9c5400f874"
      , f_name = "elders_checked_initials"
      , f_label = "Checked with Elders Initials"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 40
      , f_index = 0
      , f_subcategory = "docselect"
      , f_value = maybeBlank $ def_elders_checked_initials definitionRecord
      }
    ]

instance DMultiField Keyword where
  makeMultiField keywords = MultiField
    [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d9090183"
      , f_name = "short"
      , f_label = "Short"
      , f_head = False
      , f_reversal = True
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 5
      , f_index = 0
      , f_subcategory = "docselect"
      , f_value = maybeBlank $ short keywords
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d909181f"
      , f_name = "long"
      , f_label = "Long"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 10
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ long keywords
      }
    ]

instance DMultiField Example where
  makeMultiField exampleRecord = MultiField
    [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d905e507"
      , f_name = "example"
      , f_label = "Example"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 5
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ example exampleRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d905faf9"
      , f_name = "translation"
      , f_label = "Translation"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 10
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ translation exampleRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90612cb"
      , f_name = "example_sound"
      , f_label = "Example Sound"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 15
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ example_sound exampleRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9062573"
      , f_name = "speaker"
      , f_label = "Speaker"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 20
      , f_index = 0
      , f_subcategory = "docmultiselect"
      , f_value = maybeNull $ ex_speaker exampleRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d906381e"
      , f_name = "recording"
      , f_label = "Recording"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 25
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ ex_recording exampleRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d906fae1"
      , f_name = "timestamp"
      , f_label = "Timestamp"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 28
      , f_index = 0
      , f_subcategory = "integer"
      , f_value = maybeBlank $ ex_timestamp exampleRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9064c95"
      , f_name = "example_date"
      , f_label = "Example Date"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 30
      , f_index = 0
      , f_subcategory = "date"
      , f_value = maybeBlank $ example_date exampleRecord
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90659b5"
      , f_name = "notes"
      , f_label = "Notes on Example"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 35
      , f_index = 0
      , f_subcategory = "textarea"
      , f_value = maybeBlank $ ex_notes exampleRecord
      }
    ]

instance DMultiField InflectedForm where
  makeMultiField inflectedForm = MultiField
    [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d9082158"
      , f_name = "inflected_form"
      , f_label = "Inflected Form"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 5
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ inflected_form inflectedForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90838fa"
      , f_name = "inflection_type"
      , f_label = "Inflection Type"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 10
      , f_index = 0
      , f_subcategory = "docselect"
      , f_value = maybeBlank $ inflection_type inflectedForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9084b4a"
      , f_name = "definition"
      , f_label = "Definition"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 15
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ if_definition inflectedForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d908698a"
      , f_name = "speaker"
      , f_label = "Speaker"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 20
      , f_index = 0
      , f_subcategory = "docmultiselect"
      , f_value = maybeNull $ if_speaker inflectedForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90878da"
      , f_name = "elders_checked"
      , f_label = "Checked with Elders"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 25
      , f_index = 0
      , f_subcategory = "openboolean"
      , f_value = maybeNull $ if_elders_checked inflectedForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9088aef"
      , f_name = "elders_checked_date"
      , f_label = "Checked with Elders Date"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 30
      , f_index = 0
      , f_subcategory = "date"
      , f_value = maybeBlank $ if_elders_checked_date inflectedForm
      }
    , Field { f_identifier = "8e72d27187618f284e01dc9c540112c2"
      , f_name = "elders_checked_initials"
      , f_label = "Checked with Elders Initials"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 31
      , f_index = 0
      , f_subcategory = "docselect"
      , f_value = maybeBlank $ if_elders_checked_initials inflectedForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9089c36"
      , f_name = "sound_sample"
      , f_label = "Inflected Form Sound"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 35
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ if_sound_sample inflectedForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d908a249"
      , f_name = "recording"
      , f_label = "Recording"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 40
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ if_recording inflectedForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d908bdbf"
      , f_name = "timestamp"
      , f_label = "Timestamp"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 45
      , f_index = 0
      , f_subcategory = "integer"
      , f_value = maybeBlank $ if_timestamp inflectedForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d908d7b7"
      , f_name = "recording_date"
      , f_label = "Date of Recording"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 50
      , f_index = 0
      , f_subcategory = "date"
      , f_value = maybeBlank $ if_recording_date inflectedForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d908e571"
      , f_name = "notes"
      , f_label = "Notes of Inflected Form"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 55
      , f_index = 0
      , f_subcategory = "textarea"
      , f_value = maybeBlank $ if_notes inflectedForm
      }
    ]
    
instance DMultiField UnusedDefinition where
  makeMultiField unusedDefinition = MultiField
    [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d90a32f4"
      , f_name = "unused_definition"
      , f_label = "Unused Definition"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 5
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ unused_definition unusedDefinition
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90a3fda"
      , f_name = "source"
      , f_label = "Source"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 10
      , f_index = 0
      , f_subcategory = "docmultiselect"
      , f_value = maybeNull $ ud_source unusedDefinition
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90a546b"
      , f_name = "notes"
      , f_label = "Notes on Unused Definition"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 15
      , f_index = 0
      , f_subcategory = "textarea"
      , f_value = maybeBlank $ ud_notes unusedDefinition
      }
    ]
  
instance DMultiField OtherForm where
  makeMultiField otherForm = MultiField
    [ Field { f_identifier = "d5331cbb4d62fe3d2899f142d9092bcc"
      , f_name = "other_form"
      , f_label = "Other Form"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 5
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ other_form otherForm
      }
    , Field { f_identifier = "8e72d27187618f284e01dc9c5402d78b"
      , f_name = "form_type"
      , f_label = "Form Type"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 7
      , f_index = 0
      , f_subcategory = "docselect"
      , f_value = maybeBlank $ form_type otherForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d90944c4"
      , f_name = "source"
      , f_label = "Source"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 10
      , f_index = 0
      , f_subcategory = "docmultiselect"
      , f_value = maybeNull $ of_source otherForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9095939"
      , f_name = "sound_sample"
      , f_label = "Other Form Sound"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 15
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ of_sound_sample otherForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d9096920"
      , f_name = "speaker"
      , f_label = "Speaker"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 20
      , f_index = 0
      , f_subcategory = "docmultiselect"
      , f_value = maybeNull $ of_speaker otherForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d909854d"
      , f_name = "recording"
      , f_label = "Recording"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 25
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ of_recording otherForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d909a0c9"
      , f_name = "timestamp"
      , f_label = "Timestamp"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 30
      , f_index = 0
      , f_subcategory = "integer"
      , f_value = maybeBlank $ of_timestamp otherForm
      }
    , Field { f_identifier = "d5331cbb4d62fe3d2899f142d909ae9e"
      , f_name = "notes"
      , f_label = "Notes on Example"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 35
      , f_index = 0
      , f_subcategory = "textarea"
      , f_value = maybeBlank $ of_notes otherForm
      }
    ]

instance DMultiField VariantForm where
  makeMultiField variantForm = MultiField
    [ Field { f_identifier = "8e72d27187618f284e01dc9c54008c62"
      , f_name = "variant_form"
      , f_label = "Variant Form"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 5
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ variant_form variantForm
      }
    , Field { f_identifier = "8e72d27187618f284e01dc9c540092df"
      , f_name = "speaker"
      , f_label = "Speaker"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 10
      , f_index = 0
      , f_subcategory = "docmultiselect"
      , f_value = maybeNull $ vf_speaker variantForm
      }
    , Field { f_identifier = "8e72d27187618f284e01dc9c5400a661"
      , f_name = "dialect"
      , f_label = "Dialect"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 15
      , f_index = 0
      , f_subcategory = "docselect"
      , f_value = maybeBlank $ vf_dialect variantForm
      }
    , Field { f_identifier = "8e72d27187618f284e01dc9c5400ad96"
      , f_name = "source"
      , f_label = "Source"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 20
      , f_index = 0
      , f_subcategory = "docmultiselect"
      , f_value = maybeNull $ vf_source variantForm
      }
    , Field { f_identifier = "8e72d27187618f284e01dc9c5400bc7a"
      , f_name = "recording"
      , f_label = "Recording"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 25
      , f_index = 0
      , f_subcategory = "text"
      , f_value = maybeBlank $ vf_recording variantForm
      }
    , Field { f_identifier = "8e72d27187618f284e01dc9c5400cee3"
      , f_name = "timestamp"
      , f_label = "Timestamp"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 30
      , f_index = 0
      , f_subcategory = "integer"
      , f_value = maybeBlank $ vf_timestamp variantForm
      }
    , Field { f_identifier = "8e72d27187618f284e01dc9c5400e535"
      , f_name = "notes"
      , f_label = "Notes on Variant Form"
      , f_head = False
      , f_reversal = False
      , f_required = False
      , f_min = ""
      , f_max = ""
      , f_regex = ""
      , f_order = 35
      , f_index = 0
      , f_subcategory = "textarea"
      , f_value = maybeBlank $ vf_notes variantForm
      }
    ]
