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
 
 The purpose of this library is to allow Erlang to access functions in the
 ICU C API, primarily for making rule and locale based string comparisons
 easier.
 
 For more information, see:
 
 http://site.icu-project.org/
 
 After reading the documentation on the sites above, this file should be
 straight forward. The functions below are called by Erlang and most of the
 code simply involves converting from Erlang to C to Erlang again.
 
 The NIF function headers are uniform so no particular documentation is
 provided. See the Erlang icu module to understand the interface.
*/

#define U_CHARSET_IS_UTF8 1
#include <string.h>
#include "unicode/utypes.h" 
#include "unicode/ustring.h"
#include "unicode/ucol.h" 
#include "erl_nif.h" 
#define MAXBUFFERSIZE 100

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
get_sort_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary orig_bin;
  UChar orig [MAXBUFFERSIZE]; 
  uint8_t sort_key [MAXBUFFERSIZE];
  char locale [MAXBUFFERSIZE];
  int32_t key_size;
  UErrorCode status = U_ZERO_ERROR;
  ERL_NIF_TERM ok = enif_make_atom(env, "ok");
  UCollator *collator = 0;
  if (U_FAILURE(status)) {
    return error_helper(env, u_errorName(status));
  }
  
  if (!enif_is_list(env, argv[0])) {
    return enif_make_badarg(env);
  }
  
  (void)memset(&locale, '\0', sizeof(locale));
  
  if (enif_get_string(env, argv[0], locale, sizeof(locale), ERL_NIF_LATIN1) < 1) {
      return enif_make_badarg(env);
  }
  
  if (!enif_inspect_binary(env, argv[1], &orig_bin)) {
    return enif_make_badarg(env);
  }
  
  collator = ucol_open(locale, &status);
  if (U_FAILURE(status)) {
    ucol_close(collator);
    return error_helper(env, u_errorName(status));
  }
  
  u_uastrcpy(orig, (char *)orig_bin.data);
  
  enif_release_binary(&orig_bin);
  
  key_size = ucol_getSortKey(collator, orig, -1, sort_key, MAXBUFFERSIZE);
  
  if (U_FAILURE(status)) {
    ucol_close(collator);
    return error_helper(env, u_errorName(status));
  }
  
  ucol_close(collator);
  
  return enif_make_tuple2(env, ok, enif_make_string(env, (char*)sort_key, ERL_NIF_LATIN1));
}

static ErlNifFunc icunif_funcs[] =
{
  {"get_sort_key", 2, get_sort_key}
};

ERL_NIF_INIT(icu, icunif_funcs, load, reload, upgrade, unload)
