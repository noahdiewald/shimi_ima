/*
 Author Noah Diewald <noah@diewald.me>
 
 Copyright (c) 2010 University of Wisconsin Madison Board of Regents

 Permission is hereby granted, free of charge, to any person obtaining
 a copy of this software and associated documentation files (the
 "Software"), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish,
 distribute, sublicense, and/or sell copies of the Software, and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:

 The above copyright notice and this permission notice shall be included
 in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
 NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
 THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
*/

/*
 This is a NIF library. For more information see:

 http://www.erlang.org/doc/man/erl_nif.html

 The purpose of this library is to allow Erlang to access functions
 in the ICU C API, primarily for making rule and locale based string
 comparisons easier.

 For more information, see:

 http://site.icu-project.org/

 After reading the documentation on the sites above, this file should
 be straight forward. The functions below are called by Erlang and most
 of the code simply involves converting from Erlang to C to Erlang again.

 The NIF function headers are uniform so no particular documentation is
 provided. See the Erlang icu module to understand the interface.
*/

#include <string.h>
#include <stdint.h>
#include "unicode/utypes.h" 
#include "unicode/ustring.h"
#include "unicode/ucnv.h"
#include "unicode/ucol.h" 
#include "erl_nif.h" 
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
  This is an Erlang NIF function. See icu.erl for the Erlang interface.

  See above for more information on NIFs.

  The Erlang function icu:get_sort_key(locale, Locale, Data) when called
  will cause this funtion to be called. Locale is in argv[0] and Data
  is in argv[1].

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
  ErlNifBinary source_bin; // erlang binary containing unicode
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
  This is an Erlang NIF function. See icu.erl for the Erlang interface.

  See above for more information on NIFs.

  The Erlang function icu:get_sort_key(rule, Rule, Data) when called
  will cause this funtion to be called. Locale is in argv[0] and Data
  is in argv[1].

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
rule_sort_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary source_bin; // erlang binary containing unicode
  ErlNifBinary rule_bin; // erlang binary containing unicode
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
  
  // open collator with locale setting
  collator = ucol_openRules((UChar*)rule_bin.data, source_bin.size / 2, UCOL_DEFAULT, UCOL_DEFAULT_STRENGTH, &parse_status, &status);
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
