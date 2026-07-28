// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "symlink.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP symlinkR(SEXP tenv, SEXP tname, SEXP env, SEXP name, bool check)
{
  if(!Rf_isEnvironment(tenv)) Rf_error("`tenv` must be an environment");
  if(!Rf_isEnvironment(env))  Rf_error("`env` must be an environment");

  if(!Rf_isSymbol(tname))
  {
    if(!(Rf_isString(tname) && Rf_xlength(tname) == 1L))
    {
      Rf_error("`tname` must be a symbol or single character vector");
    }
    tname = Rf_installChar(STRING_ELT(tname, 0));
  }
  if(check && !R_existsVarInFrame(R_ParentEnv(tenv), tname))
  {
    Rf_error("`tname` does not exist in the parent environment of `tenv`");
  }

  if(!Rf_isSymbol(name))
  {
    if(!(Rf_isString(name) && Rf_xlength(name) == 1L))
    {
      Rf_error("`name` must be a symbol or single character vector");
    }
    name = Rf_installChar(STRING_ELT(name, 0));
  }
  if(check && !R_existsVarInFrame(tenv, name))
  {
    Rf_error("`name` does not exist in `tenv`");
  }
  if(check && R_existsVarInFrame(env, name))
  {
    Rf_error("`name` already exists in `env`");
  }

  PSEXP x   = Rf_install("x");
  PSEXP arg = Rf_allocList(1); SET_TAG(arg, x); SETCAR(arg, R_MissingArg);
  PSEXP bdy = Rf_lang4(
    Rf_install("if"), Rf_lang2(Rf_install("missing"), x)
   ,Rf_lang3(Rf_install("$"), tname, name)
   ,Rf_lang3(Rf_install("<-"), Rf_lang3(Rf_install("$"), tname, name), x)
  );
  PSEXP fun = R_mkClosure(arg, bdy, R_ParentEnv(tenv));
  R_MakeActiveBinding(name, fun, env);
  return Rf_ScalarLogical(1);
}

