module DictionaryDocument where

import           Text.JSON
import           Data.Typeable

class DDocument a where
  makeDocument :: a -> Document
  
class DFieldset a where
  makeFieldset :: a -> Fieldset
  
class DField a where
  makeField :: a -> Field
  
class DMultiField a where
  makeMultiField :: a -> MultiField

type OpenBool = Maybe Bool

data Document = Document
  { doctype :: String
  , description :: String
  , fieldsets :: [Fieldset]
  }
  
data Fieldset = SingleFieldset
  { s_identifier :: String
  , s_collapse :: Bool
  , s_name :: String
  , s_label :: String
  , s_order :: Integer
  , s_fields :: [Field]
  } | MultipleFieldset
  { m_identifier :: String
  , m_collapse :: Bool
  , m_name :: String
  , m_label :: String
  , m_order :: Integer
  , m_multifields :: [MultiField]
  } deriving (Show)

data Field = Field
  { f_identifier :: String
  , f_name :: String
  , f_label :: String
  , f_order :: Integer
  , f_head :: Bool
  , f_reversal :: Bool
  , f_required :: Bool
  , f_min :: String
  , f_max :: String
  , f_regex :: String
  , f_subcategory :: String
  , f_value :: JSValue
  , f_index :: Integer
  } deriving (Show)

data MultiField = MultiField [Field] deriving (Show)

instance JSON Document where
  showJSON doc = makeObj
    [ ("doctype", showJSON $ doctype doc)
    , ("description", showJSON $ description doc)
    , ("fieldsets", showJSON $ fieldsets doc)
    ]
    
instance JSON Field where
  showJSON frec = makeObj
    [ ("id", showJSON $ f_identifier frec)
    , ("name", showJSON $ f_name frec)
    , ("label", showJSON $ f_label frec)
    , ("order", showJSON $ f_order frec)
    , ("head", showJSON $ f_head frec)
    , ("reversal", showJSON $ f_reversal frec)
    , ("required", showJSON $ f_required frec)
    , ("min", showJSON $ f_min frec)
    , ("max", showJSON $ f_max frec)
    , ("regex", showJSON $ f_regex frec)
    , ("subcategory", showJSON $ f_subcategory frec)
    , ("value", f_value frec)
    , ("index", showJSON $ f_index frec)
    ]

instance JSON MultiField where
  showJSON (MultiField fields) = makeObj  [ ("fields", showJSON fields) ]

instance JSON Fieldset where  
  showJSON (SingleFieldset
    { s_identifier = identifier
    , s_name = name
    , s_label = label
    , s_order = order
    , s_collapse = collapse
    , s_fields = fields
    }) = makeObj
    [ ("id", showJSON identifier)
    , ("name", showJSON name)
    , ("label", showJSON label)
    , ("order", showJSON order)
    , ("multiple", JSBool False)
    , ("collapse", showJSON collapse)
    , ("fields" , showJSON fields)
    ]
  showJSON (MultipleFieldset
    { m_identifier = identifier
    , m_name = name
    , m_label = label
    , m_order = order
    , m_collapse = collapse
    , m_multifields = fields
    })  = makeObj
    [ ("id", showJSON identifier)
    , ("name", showJSON name)
    , ("label", showJSON label)
    , ("order", showJSON order)
    , ("multiple", JSBool True)
    , ("collapse", showJSON collapse)
    , ("multifields" , showJSON fields)
    ]

maybeBlank :: JSON a => Maybe a -> JSValue
maybeBlank Nothing = showJSON ""
maybeBlank (Just x)  = showJSON x

maybeNull :: JSON a => Maybe a -> JSValue
maybeNull Nothing = JSNull
maybeNull (Just x) = showJSON x
