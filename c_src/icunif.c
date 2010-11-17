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
#define MAXBUFFERSIZE 1024

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
  
  TODO: Move generic converter and collation creation code to separate
        function.
  TODO: Rename the incoming binary variables to something more 
        descriptive.
  TODO: Examine buffer sizes and determine whether they should be 
        a different size and whether it might not be a good idea to 
        calculate them based on what is passed in.
*/

static ERL_NIF_TERM 
locale_sort_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  /* erlang binary containing unicode string that we need a sort
     key for */
  ErlNifBinary orig_bin;
  
  /* this is the structure that will hold the return value */
  ERL_NIF_TERM out_bin;
  
  /* this will hold the sort key */
  unsigned char* out_data;
  
  /* this will hold the user suplied locale for determining
     sort order */
  char locale [MAXBUFFERSIZE];
  
  /* this holds the orig_bin after it is converted to UChar, which is the
     icu internal character type */
  UChar converted [MAXBUFFERSIZE]; 
  
  /* to hold the sort key returned by the collator */
  uint8_t key_buf [MAXBUFFERSIZE];
  
  /* to hold the sizes returned by the collator */
  int32_t key_size, conv_size;
  
  /* to hold icu error status */
  UErrorCode status = U_ZERO_ERROR;
  
  /* 'ok' to be sent back as first element in erlang tuple */
  ERL_NIF_TERM ok = enif_make_atom(env, "ok");
  
  /* pointer to the converter used to take the utf8 input and put it in the
     format used by icu */
  UConverter *conv;
  
  /* pointer to the collator */
  UCollator *collator = 0;
  
  /* Check if the first argument is a list aka string of characters */
  if (!enif_is_list(env, argv[0])) {
    return enif_make_badarg(env);
  }
  
  /* Pad the memory that will hold the string with \0 so that the string
     to be placed there is \0 terminated */
  (void)memset(&locale, '\0', sizeof(locale));
  
  /* Establish the list as a string. Erlang forces us to use latin1 but
     that is ok since locale strings are ASCII */
  if (enif_get_string(env, argv[0], locale, sizeof(locale), ERL_NIF_LATIN1) < 1)  {
    return enif_make_badarg(env);
  }
  
  /* This checks that second argument is a binary and sets it to
     orig_bin. */
  if (!enif_inspect_binary(env, argv[1], &orig_bin)) {
    return enif_make_badarg(env);
  }
  
  /* Attempt to open the converter and if an error occurs close everything
     up */
  conv = ucnv_open("utf-8", &status);
  if (U_FAILURE(status)) {
    enif_release_binary(&orig_bin);
    ucnv_close(conv);
    return error_helper(env, u_errorName(status));
  }
  
  /* Convert the original data passed in, which should be UTF8 to the
     ICU internal UTF16 that has the type UChar. */
  conv_size = ucnv_toUChars(conv, converted, MAXBUFFERSIZE, (char*)orig_bin.data, orig_bin.size, &status);
  /* TODO: it is possible to test for a buffer overrun here and allocate a
     larger buffer. 
     http://userguide.icu-project.org/conversion/converters */
  if (U_FAILURE(status)) {
    enif_release_binary(&orig_bin);
    ucnv_close(conv);
    return error_helper(env, u_errorName(status));
  }

  /* Close the converter */
  ucnv_close(conv);
  
  /* Release the binary passed in */
  enif_release_binary(&orig_bin);
  
  /* Attempt to open the collator and if an error occurs close everything
     up */
  collator = ucol_open(locale, &status);
  if (U_FAILURE(status)) {
    enif_release_binary(&orig_bin);
    ucol_close(collator);
    return error_helper(env, u_errorName(status));
  }
  
  
  /* Run the collator getSortKey funtion on the converted string. Save
     the sort key to key_buf. It should be possible to reallocate the 
     buffer if the specified one is too small but it wasn't working for
     me. http://userguide.icu-project.org/collation/api */
  key_size = ucol_getSortKey(collator, converted, conv_size, key_buf, MAXBUFFERSIZE);
  /* TODO: I am not sure if an error can be reported here */
  if (U_FAILURE(status)) {
    ucol_close(collator);
    return error_helper(env, u_errorName(status));
  }
  
  /* Close the collator */
  ucol_close(collator);
  
  /* Create a new binary to hold the sort key */
  out_data = enif_make_new_binary(env, key_size, &out_bin);
  
  /* Copy the sort key to the out_bin binary */
  memcpy(out_data, key_buf, key_size);
  
  /* Return a tuple */
  return enif_make_tuple2(env, ok, out_bin);
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
  
  TODO: Move generic converter and collation creation code to separate
        function.
  TODO: Rename the incoming binary variables to something more 
        descriptive.
  TODO: Examine buffer sizes and determine whether they should be 
        a different size and whether it might not be a good idea to 
        calculate them based on what is passed in.
*/

static ERL_NIF_TERM 
rule_sort_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  /* erlang binary containing unicode string that we need a sort
     key for */
  ErlNifBinary orig_bin;
  
  /* this is the structure that will hold the return value */
  ERL_NIF_TERM out_bin;
  
  /* this will hold the sort key */
  unsigned char* out_data;
  
  /* this will hold the user suplied rule for determining
     sort order */
  char rule [MAXBUFFERSIZE];
  
  /* this holds the orig_bin after it is converted to UChar, which is the
     icu internal character type */
  UChar converted [MAXBUFFERSIZE]; 
  
  /* to hold the sort key returned by the collator */
  uint8_t key_buf [MAXBUFFERSIZE];
  
  /* to hold the sizes returned by the collator */
  int32_t key_size, conv_size;
  
  /* to hold icu error status */
  UErrorCode status = U_ZERO_ERROR;
  
  /* 'ok' to be sent back as first element in erlang tuple */
  ERL_NIF_TERM ok = enif_make_atom(env, "ok");
  
  /* pointer to the converter used to take the utf8 input and put it in the
     format used by icu */
  UConverter *conv;
  
  /* pointer to the collator */
  UCollator *collator = 0;
  
  /* Check if the first argument is a list aka string of characters */
  if (!enif_is_list(env, argv[0])) {
    return enif_make_badarg(env);
  }
  
  /* Pad the memory that will hold the string with \0 so that the string
     to be placed there is \0 terminated */
  (void)memset(&rule, '\0', sizeof(rule));
  
  /* Establish the list as a string. Erlang forces us to use latin1 but
     that is ok since rule strings are (in this program) ASCII */
  if (enif_get_string(env, argv[0], rule, sizeof(rule), ERL_NIF_LATIN1) < 1)  {
    return enif_make_badarg(env);
  }
  
  /* This checks that second argument is a binary and sets it to
     orig_bin. */
  if (!enif_inspect_binary(env, argv[1], &orig_bin)) {
    return enif_make_badarg(env);
  }
  
  /* Attempt to open the converter and if an error occurs close everything
     up */
  conv = ucnv_open("utf-8", &status);
  if (U_FAILURE(status)) {
    enif_release_binary(&orig_bin);
    ucnv_close(conv);
    return error_helper(env, u_errorName(status));
  }
  
  /* Convert the original data passed in, which should be UTF8 to the
     ICU internal UTF16 that has the type UChar. */
  conv_size = ucnv_toUChars(conv, converted, MAXBUFFERSIZE, (char*)orig_bin.data, orig_bin.size, &status);
  /* TODO: it is possible to test for a buffer overrun here and allocate a
     larger buffer. 
     http://userguide.icu-project.org/conversion/converters */
  if (U_FAILURE(status)) {
    enif_release_binary(&orig_bin);
    ucnv_close(conv);
    return error_helper(env, u_errorName(status));
  }

  /* Close the converter */
  ucnv_close(conv);
  
  /* Release the binary passed in */
  enif_release_binary(&orig_bin);
  
  /* Attempt to open the collator and if an error occurs close everything
     up */
  collator = ucol_open(rule, &status);
  if (U_FAILURE(status)) {
    enif_release_binary(&orig_bin);
    ucol_close(collator);
    return error_helper(env, u_errorName(status));
  }
  
  /* Run the collator getSortKey funtion on the converted string. Save
     the sort key to key_buf. It should be possible to reallocate the 
     buffer if the specified one is too small but it wasn't working for
     me. http://userguide.icu-project.org/collation/api */
  key_size = ucol_getSortKey(collator, converted, conv_size, key_buf, MAXBUFFERSIZE);
  /* TODO: I am not sure if an error can be reported here */
  if (U_FAILURE(status)) {
    ucol_close(collator);
    return error_helper(env, u_errorName(status));
  }
  
  /* Close the collator */
  ucol_close(collator);
  
  /* Create a new binary to hold the sort key */
  out_data = enif_make_new_binary(env, key_size, &out_bin);
  
  /* Copy the sort key to the out_bin binary */
  memcpy(out_data, key_buf, key_size);
  
  /* Return a tuple */
  return enif_make_tuple2(env, ok, out_bin);
}

static ErlNifFunc icunif_funcs[] =
{
  {"locale_sort_key", 2, locale_sort_key},
  {"rule_sort_key", 2, rule_sort_key}
};

ERL_NIF_INIT(icu, icunif_funcs, load, reload, upgrade, unload)
