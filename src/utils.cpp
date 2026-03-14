// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "utils.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP isname(SEXP x, SEXP names)
{
  if(!(TYPEOF(x) == SYMSXP && TYPEOF(names) == STRSXP))
  {
    return Rf_ScalarLogical(0);
  }
  const R_xlen_t n = Rf_xlength(names);
  if(n == 0)
  {
    return Rf_ScalarLogical(1);
  }
  const char *name = CHAR(PRINTNAME(x));
  for(R_xlen_t i = 0; i < n; ++i)
  {
    if(strcmp(CHAR(STRING_ELT(names, i)), name) == 0)
    {
      return Rf_ScalarLogical(1);
    }
  }
  return Rf_ScalarLogical(0);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP iscall(SEXP x, SEXP names)
{
  if(TYPEOF(x) != LANGSXP)
  {
    return Rf_ScalarLogical(0);
  }
  return isname(CAR(x), names);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP symlink(SEXP tenv, SEXP tname, SEXP env, SEXP name)
{
  if(!Rf_isEnvironment(tenv)) Rf_error("`tenv` must be an environment");
  if(!Rf_isEnvironment(env))  Rf_error("`env` must be an environment");

  if(!Rf_isSymbol(tname))
  {
    if(!(Rf_isString(tname) && Rf_length(tname) == 1L))
    {
      Rf_error("`tname` must be a symbol or single character vector");
    }
    tname = Rf_installChar(STRING_ELT(tname, 0));
    if(!R_existsVarInFrame(ENCLOS(tenv), tname))
    {
      Rf_error("`tname` does not exist in the parent environment of `tenv`");
    }
  }

  if(!Rf_isSymbol(name))
  {
    if(!(Rf_isString(name) && Rf_length(name) == 1L))
    {
      Rf_error("`name` must be a symbol or single character vector");
    }
    name = Rf_installChar(STRING_ELT(name, 0));

    if(!R_existsVarInFrame(tenv, name))
    {
      Rf_error("`name` does not exist in `tenv`");
    }
    if(R_existsVarInFrame(env, name))
    {
      Rf_error("`name` already exists in `env`");
    }
  }

  pSEXP x   = Rf_install("x");
  pSEXP fun = Rf_allocSExp(CLOSXP);
  pSEXP arg = Rf_allocList(1); SET_TAG(arg, x); SETCAR(arg, R_MissingArg);
  pSEXP bdy = Rf_lang4(
    Rf_install("if"), Rf_lang2(Rf_install("missing"), x)
   ,Rf_lang3(Rf_install("$"), tname, name)
   ,Rf_lang3(Rf_install("<-"), Rf_lang3(Rf_install("$"), tname, name), x)
  );
  SET_FORMALS(fun, arg);
  SET_BODY(fun, bdy);
  SET_CLOENV(fun, ENCLOS(tenv));
  R_MakeActiveBinding(name, fun, env);
  return Rf_ScalarLogical(1);
}
