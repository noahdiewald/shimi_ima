% Types for use with CouchDB

-record(vq, {
  key :: jsn:json_term(), 
  startkey :: jsn:json_term(), 
  startkey_docid :: string(), 
  endkey :: jsn:json_term(), 
  endkey_docid :: string(),
  limit :: jsn:json_number(), 
  stale :: ok | update_after, 
  descending = false :: boolean(), 
  skip = 0 :: jsn:json_number(), 
  group_level = exact :: jsn:json_number() | exact, 
  reduce = true :: boolean(), 
  include_docs = false :: boolean(), 
  inclusive_end = true :: boolean(),
  update_seq = false :: boolean()
}).

-type view_query() :: #vq{}.

% Types for use with frequently used structures

-type regex() :: binary().

-record(charseq, {
  id :: binary(),
  rev :: binary(),
  category :: charseq,
  description :: binary(),
  characters :: [binary()],
  name :: binary(),
  sort_ignore :: [regex()],
  locale :: string(),
  tailoring ::  icu:ustring(),
  vowels :: [binary()],
  consonants :: [binary()],
  ietf_tag :: binary(),
  iso639_tag :: binary()
}).

-type charseq() :: #charseq{}.

-type subcategory() :: text | textarea | date | integer | rational | boolean | openboolean | select | multiselect | docselect | docmultiselect | file.

-type dateval() :: calendar:date() | today.

-type basicval() :: number() | binary() | calendar:date() | null.

-record(field, {
  id :: binary(),
  rev :: binary(),
  allowed :: [basicval()],
  category :: field,
  charseq :: binary(),
  default :: basicval(),
  description :: binary(),
  doctype :: binary(),
  fieldset :: binary(),
  head :: boolean(),
  label :: boolean(),
  max :: basicval(),
  min :: basicval(),
  name :: boolean(),
  order :: integer(),
  regex :: regex(),
  required :: boolean(),
  reversal :: boolean(),
  source :: binary(),
  subcategory :: subcategory()
}).

-record(fieldset, {
  id :: binary(),
  rev :: binary(),
  category :: fieldset,
  description :: binary(),
  doctype :: binary(),
  label :: boolean(),
  name :: boolean(),
  order :: integer(),
  multiple :: boolean(),
  collapse :: boolean()
}).

-record(doctype, {
  id :: binary(),
  rev :: binary(),
  category :: doctype,
  description :: binary()
}).

-type field() :: #field{}.
-type fieldset() :: #fieldset{}.
-type doctype() :: #doctype{}.

-type anyval() :: basicval() | [binary()] | boolean() | null.

-record(docfield, {
  id :: binary(),
  instance :: binary(),
  charseq :: binary(),
  head :: boolean(),
  label :: boolean(),
  max :: basicval(),
  min :: basicval(),
  name :: boolean(),
  order :: integer(),
  regex :: regex(),
  required :: boolean(),
  reversal :: boolean(),
  subcategory :: subcategory(),
  value :: anyval(),
  sortkey :: binary()
}).

-type docfield() :: #docfield{}.

-record(docfieldset, {
  id :: binary(),
  label :: boolean(),
  name :: boolean(),
  order :: integer(),
  multiple :: boolean(),
  collapse :: boolean(),
  fields :: [docfield()] | [[docfield()]]
}).

-type docfieldset() :: #docfieldset{}.

-record(document, {
  id :: binary(),
  rev :: binary(),
  description :: binary(),
  doctype :: binary(),
  created_at :: calendar:datetime(),
  created_by :: binary(),
  updated_at :: calendar:datetime(),
  updated_by :: binary(),
  prev :: binary(),
  deleted :: boolean(),
  fieldsets :: [docfieldset()]
}).

-type document() :: #document{}.
