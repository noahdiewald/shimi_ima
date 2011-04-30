%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 University of Wisconsin Madison Board of Regents.
%% Copyright (c) 2010 University of Wisconsin Madison Board of Regents
%%
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
%% THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% @doc A resource to allow the abstraction of script loading to allow better
%% organization of script files as well as versatility in minimization or
%% optimization. This will also eventually allow for the loading of custom,
%% user supplied, scripts that are created through the web user interface and
%% saved to the database.

-module(javascript_resource).
-export([
  content_types_provided/2,
  init/1,
  resource_exists/2,
  to_js/2
]).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

init(S) -> {ok, S}.

resource_exists(R, S) ->
  Path = wrq:path_info(script, R),
  case script_exists(S, Path) of 
    {true, NamePath} -> {true, R, [{path, NamePath}|S]};
    _ -> {false, R, S}
  end.

content_types_provided(R, S) ->
  {[{"text/javascript", to_js}], R, S}.

to_js(R, S) ->
  Fun = fun (File, Acc) -> 
    {ok, Content} = file:read_file(File),
    [Content|Acc]
  end,
  Pattern = "\.js$",
  Common = filelib:fold_files(proplists:get_value(root, S), Pattern, false, Fun, []),
  Script = filelib:fold_files(proplists:get_value(path, S), Pattern, true, Fun, []),
  {[Common|Script], R, S}.
  
script_exists(S, Name) ->
  NamePath = script_path(S, Name),
  case filelib:is_dir(NamePath) of 
    true -> {true, NamePath};
    false -> false
  end.

script_path(S, Name) ->
  RelName = case hd(Name) of
    "/" -> tl(Name);
    _ -> Name
  end,
  filename:join([proplists:get_value(root, S), RelName]).
