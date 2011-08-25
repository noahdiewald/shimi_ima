module Nouns where

data CSVNoun = CSVNoun 
  { headword :: String
  , speaker :: String
  , lexical_category :: String
  , underlying_representation :: String
  , definitions :: [Definition]
  , plural
convertToJson