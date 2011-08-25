module Nouns where

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
  ] = encode $ makeDocument Entry
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
    , codes = Just ["IMPORT"]
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
      { inflected_form = maybeString cplural
      , inflection_type = Just "plural"
      , if_definition = Nothing
      , if_speaker = maybeList cplural_speaker
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
      { inflected_form = maybeString cposs
      , inflection_type = Just "poss"
      , if_definition = Nothing
      , if_speaker = maybeList cposs_speaker
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
      { inflected_form = maybeString c3poss
      , inflection_type = Just "3poss"
      , if_definition = Nothing
      , if_speaker = maybeList c3poss_speaker
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
      { inflected_form = maybeString cdim
      , inflection_type = Just "dim"
      , if_definition = Nothing
      , if_speaker = maybeList cdim_speaker
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
      { inflected_form = maybeString cpej
      , inflection_type = Just "pej"
      , if_definition = Nothing
      , if_speaker = maybeList cpej_speaker
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
      { inflected_form = maybeString cloc
      , inflection_type = Just "loc"
      , if_definition = Nothing
      , if_speaker = maybeList cloc_speaker
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
      { inflected_form = maybeString cobv
      , inflection_type = Just "obv"
      , if_definition = Nothing
      , if_speaker = maybeList cobv_speaker
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
      { inflected_form = maybeString cvoc
      , inflection_type = Just "voc"
      , if_definition = Nothing
      , if_speaker = maybeList cvoc_speaker
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
