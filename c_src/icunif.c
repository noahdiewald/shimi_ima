/*
*/

#include "unicode/ucol.h"
#include "erl_nif.h"
#define MAXBUFFERSIZE 100

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static void unload(ErlNifEnv* env, void* priv)
{
  return;
}

static ERL_NIF_TERM get_sort_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  UErrorCode status = U_ZERO_ERROR;
  UCollator *myCollator = 0;
  if (U_FAILURE(status)) {
    return enif_make_atom(env, "error");
  }
  
  myCollator = ucol_open("", &status);
  if (U_FAILURE(status)) {
    ucol_close(myCollator);
    return enif_make_atom(env, "error");
  }
  
  ucol_close(myCollator);
  
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_string(env, "fake", ERL_NIF_LATIN1));
}

static ErlNifFunc icunif_funcs[] =
{
  {"get_sort_key", 0, get_sort_key}
};

ERL_NIF_INIT(icu, icunif_funcs, load, reload, upgrade, unload)
