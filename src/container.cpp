// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "container.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP oopr_cont_init(SEXP ooprC, SEXP thiz, SEXP map)
{
  if(!is_ooprC(ooprC))        Rf_error("`ooprC` must be an ooprC object");
  if(!Rf_isEnvironment(thiz)) Rf_error("`thiz` not an environment");
  if(!Rf_isLogical(map))      Rf_error("`map` must be logical");

  SEXP fun = Rf_findVarInFrame(thiz, Rf_install("emplace"));

  // create new formals from ooprC constructor method
  pSEXP arg;
  if(LOGICAL_ELT(map, 0))
  {
    arg = R_MissingArg;
  }
  else
  {
    arg = Rf_lang3(Rf_install("$"), Rf_install("this"), Rf_install("size"));
  }
  pSEXP formals = CONS(arg, FORMALS(ooprC));
  SET_TAG(formals, Rf_install("."));
  SET_FORMALS(fun, formals);

  // collect argument names and substitute
  SEXP args    = FORMALS(ooprC);
  const R_xlen_t len = Rf_xlength(args);
  pSEXP sub    = Rf_allocVector(VECSXP, len);
  R_xlen_t i = 0;
  for(SEXP e = args; e != R_NilValue; e = CDR(e), ++i)
  {
    SET_VECTOR_ELT(sub, i, TAG(e));
  }
  pSEXP env = R_NewEnv(R_EmptyEnv, 1, (int)len);
  Rf_defineVar(Rf_install("args"), sub, env);
  SET_BODY(fun, Rf_substitute(BODY(fun), env));

  return Rf_ScalarLogical(1);
}
