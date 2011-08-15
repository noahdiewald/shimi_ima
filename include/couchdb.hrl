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
