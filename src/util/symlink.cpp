// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "symlink.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP symlinkR(SEXP tenv, SEXP tname, SEXP env, SEXP name, bool check)
{
  if(!REnv<>::is(tenv)) Rf_error("`tenv` must be an environment");
  if(!REnv<>::is(env))  Rf_error("`env` must be an environment");
  const REnv<SEXP> tenvir(tenv);
  REnv<SEXP>       envir(env);

  struct make { static RSym sym(SEXP x, const char* nm)
  {
    if(RSym::is(x)) return x;
    if(RChr<>::is(x))
    {
      const RChr<SEXP> chr(x);
      if(chr.size() == 1) return chr[0].sym();
    }
    Rf_error("`%s` must be a symbol or single character vector", nm);
  }};
  const RSym tsym(make::sym(tname, "tname"));
  const RSym sym(make::sym(name, "name"));

  if(check && !tenvir.parent()[tsym].exists())
  {
    Rf_error("`tname` does not exist in the parent environment of `tenv`");
  }
  if(check && !tenvir[sym].exists())
  {
    Rf_error("`name` does not exist in `tenv`");
  }
  if(check && envir[sym].exists())
  {
    Rf_error("`name` already exists in `env`");
  }

  RSym x("x");
  PSEXP arg = Rf_allocList(1); SET_TAG(arg, x); SETCAR(arg, R_MissingArg);
  PSEXP bdy = Rf_lang4(
    RSym("if"), Rf_lang2(RSym("missing"), x)
   ,Rf_lang3(RSym("$"), tsym, sym)
   ,Rf_lang3(RSym("<-"), Rf_lang3(RSym("$"), tsym, sym), x)
  );

  envir[sym].fun(R_mkClosure(arg, bdy, tenvir.parent()));
  return Rf_ScalarLogical(1);
}

