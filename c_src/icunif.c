/*
*/

#define U_CHARSET_IS_UTF8 1
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
  int32_t key_size;
  UErrorCode status = U_ZERO_ERROR;
  ERL_NIF_TERM ok = enif_make_atom(env, "ok");
  UCollator *collator = 0;
  if (U_FAILURE(status)) {
    return error_helper(env, u_errorName(status));
  }
  
  if (!enif_inspect_binary(env, argv[0], &orig_bin)) {
    return enif_make_badarg(env);
  }
  
  collator = ucol_open("", &status);
  if (U_FAILURE(status)) {
    ucol_close(collator);
    return error_helper(env, u_errorName(status));
  }
  
  u_uastrcpy(orig, (char *)orig_bin.data);
  
  enif_release_binary(&orig_bin);
  
  key_size = ucol_getSortKey(collator, orig, orig_bin.size, sort_key, MAXBUFFERSIZE);
  
  if (U_FAILURE(status)) {
    ucol_close(collator);
    return error_helper(env, u_errorName(status));
  }
  
  ucol_close(collator);
  
  return enif_make_tuple2(env, ok, enif_make_string(env, (char*)sort_key, ERL_NIF_LATIN1));
}

static ErlNifFunc icunif_funcs[] =
{
  {"get_sort_key", 1, get_sort_key}
};

ERL_NIF_INIT(icu, icunif_funcs, load, reload, upgrade, unload)
