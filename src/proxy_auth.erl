%%% Copyright 2012 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of Ʃimi Ima.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General
%%% Public License along with dictionary_maker. If not, see
%%% <http://www.gnu.org/licenses/>.

%%% @copyright 2012 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc Simple proxying of basic authentication to CouchDB

-module(proxy_auth).
-author('Noah Diewald <noah@diewald.me>').

-export([
         is_authorized/2
        ]).

is_authorized(R, S) ->
    S1 = [{auth_head, "Basic realm=dictionary"}|S],
    {Auth, R1} = cowboy_req:header(<<"authorization">>, R),
    case binary_to_list(Auth) of
        "Basic " ++ Base64 ->
            S2 = update_client_headers({"Authorization", "Basic " ++ Base64}, S1),
            do_basic_authentication(Base64, R1, S2);
        _ -> {proplists:get_value(auth_head, S1), R1, S1}
    end.

do_basic_authentication(Base64, R, S) ->
    Str = base64:mime_decode_to_string(Base64),
    case string:tokens(Str, ":") of
        [Username, Password] -> couchdb_authenticate(Username, Password, R, S);
        _ -> {proplists:get_value(auth_head, S), R, S}
    end.

couchdb_authenticate(Username, Password, R, S) ->
    Body = "name=" ++ Username ++ "&password=" ++ Password,
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Resp = ibrowse:send_req(utils:ndb() ++ "_session", Headers, post, Body),
    case Resp of
        {ok, "200", _, Json} -> 
            do_validations(jsn:decode(Json), R, S);
        {ok, "401", _, _} -> {proplists:get_value(auth_head, S), R, S}
    end.

do_validations(Struct, R, S) ->
    Mod = proplists:get_value(source_mod, S),
    {Valid, R1, S1} = Mod:validate_authentication(Struct, R, S),
    Name = jsn:get_value(<<"name">>, Struct),
    Roles = jsn:get_value(<<"roles">>, Struct),
    {Valid, R1, [{user, [{name, Name}, {roles, Roles}]}|S1]}.


update_client_headers(Header, S) ->
    case proplists:get_value(headers, S) of
        undefined -> [{headers, [Header]}|S];
        Headers -> 
            S1 = proplists:delete(headers, S),
            [{headers, [Header|Headers]}|S1]
    end.
