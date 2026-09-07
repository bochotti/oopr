// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "symlink.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#define LIST(X)                                                \
  X(x)                                                         \
  X(iff, "if")                                                 \
  X(missing)                                                   \
  X(dollar, "$")                                               \
  X(assign, "<-")
SYMBOLS(LIST, s)
#undef  LIST
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP symlinkR(SEXP tenv, SEXP tname, SEXP env, SEXP name) try
{
  return symlinkR(tenv, tname, env, name, true);
}
catchR
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP symlinkR(SEXP tenv, SEXP tname, SEXP env, SEXP name, bool check)
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

  PSEXP arg = Rf_allocList(1); SET_TAG(arg, *s.x); SETCAR(arg, R_MissingArg);
  PSEXP bdy = Rf_lang4(
    *s.iff, Rf_lang2(*s.missing, *s.x)
   ,Rf_lang3(*s.dollar, *tsym, *sym)
   ,Rf_lang3(*s.assign, Rf_lang3(*s.dollar, *tsym, *sym), *s.x)
  );

  envir[sym].fun = R_mkClosure(arg, bdy, *tenvir.parent());
  return Rf_ScalarLogical(1);
}
