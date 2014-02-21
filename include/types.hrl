-type regex() :: binary().
-type subcategory() :: text | textarea | date | integer | rational | boolean | 
                       openboolean | select | multiselect | docselect | 
                       docmultiselect | file.
-type dateval() :: calendar:date() | today.
-type basicval() :: number() | binary() | calendar:date() | null.
-type anyval() :: basicval() | [binary()] | boolean() | null.
-type instance() :: binary().
-type sortkey() :: binary().
-type fieldid() :: binary().
-type fieldsetid() :: binary().
-type doctypeid() :: binary().
-type documentid() :: binary().
-type charseqid() :: binary().
-type rev() :: binary().
-type text() :: binary(). % for longer text
-type bstring() :: binary(). % binary form of string
-type charstring() :: binary(). % this is a string representing a character
-type sortkey_val() :: [sortkey() | anyval()]. % this should be a two item list
-type view_update_list() :: [{binary(), integer()}].

-record(maint_doc, {
          view_update_info :: view_update_list()}).

-record(db_info, {
          db_name :: binary(),
          doc_count :: integer(),
          doc_del_count :: integer(),
          update_seq :: integer(),
          purge_seq :: integer(),
          compact_running :: boolean(),
          disk_size :: integer(),
          data_size :: integer(),
          instance_start_time :: binary(),
          disk_format_version :: integer(),
          committed_update_seq :: integer()}).
          
-record(vq, {
          key :: jsn:json_term(), 
          keys :: jsn:json_term(), 
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

-record(charseq, {
          id :: charseqid(),
          rev :: rev(),
          category :: charseq,
          description :: text(),
          characters :: [charstring()],
          name :: bstring(),
          sort_ignore :: [regex()],
          locale :: string(),
          tailoring :: icu:ustring(),
          vowels :: [charstring()],
          consonants :: [charstring()],
          ietf_tag :: bstring(),
          iso639_tag :: bstring()
         }).

-record(field, {
          id :: fieldid(),
          rev :: rev(),
          allowed :: [basicval()] | null,
          category :: field,
          charseq :: charseqid(),
          default :: basicval(),
          description :: text(),
          doctype :: doctypeid(),
          fieldset :: fieldsetid(),
          head :: boolean(),
          label :: bstring(),
          max :: basicval(),
          min :: basicval(),
          name :: bstring(),
          order :: integer(),
          regex :: regex(),
          required :: boolean(),
          reversal :: boolean(),
          source :: doctypeid(),
          subcategory :: subcategory()
         }).

-record(fieldset, {
          id :: fieldsetid(),
          rev :: rev(),
          category :: fieldset,
          description :: text(),
          doctype :: doctypeid(),
          label :: bstring(),
          name :: bstring(),
          order :: integer(),
          multiple :: boolean(),
          collapse :: boolean(),
          fields :: [field()]
         }).

-record(doctype, {
          id :: doctypeid(),
          rev :: rev(),
          name :: bstring(),
          category :: doctype,
          description :: text(),
          fieldsets :: [fieldset()]
         }).

-record(docfield, {
          id :: fieldid(),
          instance :: instance(),
          charseq :: charseqid(),
          head :: boolean(),
          label :: bstring(),
          max :: basicval(),
          min :: basicval(),
          name :: bstring(),
          order :: integer(),
          regex :: regex(),
          required :: boolean(),
          reversal :: boolean(),
          subcategory :: subcategory(),
          value :: anyval(),
          sortkey :: sortkey()
         }).

-record(docfieldset, {
          id :: fieldsetid(),
          label :: bstring(),
          name :: bstring(),
          order :: integer(),
          multiple :: boolean(),
          collapse :: boolean(),
          fields :: [docfield()] | [[docfield()]]
         }).

-record(document, {
          id :: documentid(),
          rev :: rev(),
          description :: text(),
          doctype :: doctypeid(),
          created_at :: calendar:datetime(),
          created_by :: bstring(),
          updated_at :: calendar:datetime(),
          updated_by :: bstring(),
          prev :: rev(),
          deleted :: boolean(),
          fieldsets :: [docfieldset()],
          index :: [{fieldid(),sortkey_val() | [sortkey_val()]}],
          changes :: [{instance(), [{binary(), jsn:json_term()}]}] | null,
          head :: [fieldid()],
          reverse :: [fieldid()]
         }).

-type view_query() :: #vq{}.
-type charseq() :: #charseq{}.
-type field() :: #field{}.
-type fieldset() :: #fieldset{}.
-type doctype() :: #doctype{}.
-type docfield() :: #docfield{}.
-type docfieldset() :: #docfieldset{}.
-type document() :: #document{}.

-record(sparams, {
          doctype :: string(),
          index :: string(),
          fields :: [binary()],
          exclude :: boolean(),
          invert :: boolean(),
          qs :: string()
         }).

-type sparams() :: #sparams{}.
