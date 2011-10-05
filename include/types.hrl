% Types for use with CouchDB

-record(vq, {key :: jsn:json_term(), 
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
             update_seq = false :: boolean()}).

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
