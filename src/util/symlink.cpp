// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "symlink.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP symlinkR(SEXP tenv, SEXP tname, SEXP env, SEXP name, bool check) try
{
  if(!REnv<>::is(tenv)) stop("`tenv` must be an environment");
  if(!REnv<>::is(env))  stop("`env` must be an environment");
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
    stop("`%s` must be a symbol or single character vector", nm);
    return R_NilValue;
  }};
  const RSym tsym(make::sym(tname, "tname"));
  const RSym sym(make::sym(name, "name"));

  if(check && !tenvir.parent()[tsym].exists())
  {
    stop("`tname` does not exist in the parent environment of `tenv`");
  }
  if(check && !tenvir[sym].exists())
  {
    stop("`name` does not exist in `tenv`");
  }
  if(check && envir[sym].exists())
  {
    stop("`name` already exists in `env`");
  }

  RSym x("x");
  PSEXP arg = Rf_allocList(1); SET_TAG(arg, x); SETCAR(arg, R_MissingArg);
  PSEXP bdy = Rf_lang4(
    RSym("if"), Rf_lang2(RSym("missing"), x)
   ,Rf_lang3(RSym("$"), tsym, sym)
   ,Rf_lang3(RSym("<-"), Rf_lang3(RSym("$"), tsym, sym), x)
  );

  envir[sym].fun = R_mkClosure(arg, bdy, tenvir.parent());
  return Rf_ScalarLogical(1);
}
catchR
