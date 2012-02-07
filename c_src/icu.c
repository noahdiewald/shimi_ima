/* -------------------------------------------------------------------
 * icu: Erlang NIF library for ICU collation
 * 
 * Copyright 2011 University of Wisconsin Madison Board of Regents.
 * Author Noah Diewald <noah@diewald.me>
 *
 * This file is part of dictionary_maker.
 *
 * dictionary_maker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * dictionary_maker is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with dictionary_maker. If not, see <http://www.gnu.org/licenses/>.
 *
 * -------------------------------------------------------------------
 */

/*
 * NIF implementations for icu.erl
 *
 * For more information on NIFs see the erlang documentation.
 *
 * All binaries passed to these functions should be encoded using
 * icu4e's ustring functions.
 */
#include <string.h>
#include "erl_nif.h"
#include "unicode/ustring.h"
#include "unicode/utypes.h"
#include "unicode/uloc.h"
#include "unicode/ucol.h"
#define BUFFERSIZE 1024

static int 
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static int 
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static int 
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static void 
unload(ErlNifEnv* env, void* priv)
{
  return;
}

static ERL_NIF_TERM 
error_helper(ErlNifEnv* env, const char* message)
{
  ERL_NIF_TERM error_atm = enif_make_atom(env, "error");
  ERL_NIF_TERM error_msg = enif_make_string(env, message, ERL_NIF_LATIN1);
  
  return enif_make_tuple2(env, error_atm, error_msg);
}

static ERL_NIF_TERM 
get_sort_key(ErlNifEnv* env, const UCollator* coll, const UChar* source, uint32_t srcsize)
{
  ERL_NIF_TERM result_bin; // erlang binary return value
  uint8_t* result; 
  uint8_t buffer [BUFFERSIZE]; // stack buffer
  uint8_t* currBuffer = buffer;
  int32_t bufferLen = sizeof(buffer);
  int32_t expectedLen = 0;
  
  expectedLen = ucol_getSortKey(coll, source, srcsize, currBuffer, bufferLen);
  if (expectedLen > bufferLen) {
    if (currBuffer == buffer) {
      currBuffer = malloc(expectedLen);
    } else {
      currBuffer = realloc(currBuffer, expectedLen);
    }
  }
  bufferLen = ucol_getSortKey(coll, source, srcsize, currBuffer, expectedLen);
  
  result = enif_make_new_binary(env, bufferLen, &result_bin);
  memcpy(result, currBuffer, bufferLen);
  
  if (currBuffer != buffer && currBuffer != NULL) {
    free(currBuffer);
  }
  
  return result_bin;
}

/*
  This is an Erlang NIF function. See icu.erl for the Erlang
  interface.

  See above for more information on NIFs.

  The Erlang function icu:get_sort_key(locale, Locale, Data) when
  called will cause this funtion to be called. Locale is in argv[0]
  and Data is in argv[1].

  These values will be finesed until they can be used to open an ICU
  collator with the given Locale defining a sort order. The Data will
  then be passed to the ICU collation get_sortKey function to get a
  key that can be used to collate values using normal string or list
  comparison functions.

  The value returned is the NIF representation of an Erlang tuple.

  If an error occurs and is caught the value will come back to Erlang
  as {error, Reason} where Reason is a latin1 string.

  Otherwise {ok, SortKey} where SortKey is an Erlang binary.
*/

static ERL_NIF_TERM 
locale_sort_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary source_bin; // erlang binary containing ustring
  ERL_NIF_TERM result_bin; // erlang binary return value
  char locale [256] = { '\0' }; // for user supplied locale
  UErrorCode status = U_ZERO_ERROR; // error status
  ERL_NIF_TERM ok = enif_make_atom(env, "ok"); // atom for return value
  UCollator *collator = 0; // icu collator
  
  // Check if the first argument is an erlang list
  if (!enif_is_list(env, argv[0])) {
    return enif_make_badarg(env);
  }
  
  // check that term is an erlang string
  if (enif_get_string(env, argv[0], locale, sizeof(locale), ERL_NIF_LATIN1) < 1)  {
    return enif_make_badarg(env);
  }
  
  // check and initialize binary
  if (!enif_inspect_binary(env, argv[1], &source_bin)) {
    return enif_make_badarg(env);
  }
  
  // open collator with locale setting
  collator = ucol_open(locale, &status);
  if (U_FAILURE(status)) {
    enif_release_binary(&source_bin);
    ucol_close(collator);
    return error_helper(env, u_errorName(status));
  }
  
  result_bin = get_sort_key(env, collator, (UChar*)source_bin.data, source_bin.size / 2);
  
  enif_release_binary(&source_bin);
  ucol_close(collator);
  
  return enif_make_tuple2(env, ok, result_bin);
}

/*
 *  The Erlang function icu:get_sort_key(rule, Rule, Data) when called
 *  will cause this funtion to be called. Rule and Data are both binaries
 *  containing UStrings. Rule is in argv[0] and Data is in argv[1].
 *
 *  These values will be used to open an ICU collator with the given
 *  Rule defining a sort order. The Data will then be passed to the
 *  ICU collation get_sortKey function to get a key that can be used to
 *  collate values using normal string or list comparison functions.
 *
 *  The value returned is the NIF representation of an Erlang tuple.
 *
 *  If an error occurs and is caught the value will come back to Erlang
 *  as {error, Reason} where Reason is a latin1 string.
 *
 *  Otherwise {ok, SortKey} where SortKey is an Erlang binary.
*/

static ERL_NIF_TERM 
rule_sort_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary source_bin; // erlang binary containing UString
  ErlNifBinary rule_bin; // erlang binary containing UString
  ERL_NIF_TERM result_bin; // erlang binary return value
  UErrorCode status = U_ZERO_ERROR; // error status
  UParseError parse_status; // error status
  ERL_NIF_TERM ok = enif_make_atom(env, "ok"); // atom for return value
  UCollator *collator = 0; // icu collator
  
  // check and initialize binaries
  if (!enif_inspect_binary(env, argv[0], &rule_bin)) {
    return enif_make_badarg(env);
  }
  
  if (!enif_inspect_binary(env, argv[1], &source_bin)) {
    return enif_make_badarg(env);
  }
  
  // open collator
  collator = ucol_openRules((UChar*)rule_bin.data, rule_bin.size / 2, UCOL_DEFAULT, UCOL_DEFAULT_STRENGTH, &parse_status, &status);
  if (U_FAILURE(status)) {
    enif_release_binary(&source_bin);
    ucol_close(collator);
    return error_helper(env, u_errorName(status));
  }
  
  result_bin = get_sort_key(env, collator, (UChar*)source_bin.data, source_bin.size / 2);
  
  enif_release_binary(&rule_bin);
  enif_release_binary(&source_bin);
  ucol_close(collator);
  
  return enif_make_tuple2(env, ok, result_bin);
}

static ErlNifFunc icunif_funcs[] =
{
  {"locale_sort_key", 2, locale_sort_key},
  {"rule_sort_key", 2, rule_sort_key}
};

ERL_NIF_INIT(icu, icunif_funcs, load, reload, upgrade, unload)
