// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "enclosure.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP interface(SEXP env, SEXP nme, SEXP nms, SEXP cls)
{
  if(!Rf_isEnvironment(env)) Rf_error("`env` must be an environment");
  if(Rf_isNull(nms))
  {
    nms = R_lsInternal3(env, TRUE, FALSE);
  }
  else if(!Rf_isString(nms))
  {
    Rf_error("`nms` must be a character vector");
  }
  if(Rf_isNull(cls))
  {
    cls = Rf_getAttrib(env, R_ClassSymbol);
  }
  else if(!Rf_isString(cls))
  {
    Rf_error("`cls` must be a character vector");
  }

  const R_xlen_t len = Rf_xlength(nms);
  pSEXP out = R_NewEnv(R_ParentEnv(env), 1, (int)len);
  Rf_setAttrib(out, R_ClassSymbol, cls);

  for(R_xlen_t i = 0; i < len; ++i)
  {
    SEXP mem, nm = STRING_ELT(nms, i), sym = Rf_installChar(nm);
    if(R_BindingIsActive(sym, env))
    {
      mem = R_ActiveBindingFunction(sym, env);
      R_MakeActiveBinding(sym, mem, out);
    }
    else
    {
      mem = R_getVarEx(sym, env, FALSE, R_NilValue);
      if(Rf_isFunction(mem))
      {
        Rf_defineVar(sym, mem, out);
      }
      else
      {
        symlinkR(env, nme, out, sym);
      }
    }
    if(R_BindingIsLocked(sym, env)) R_LockBinding(sym, out);
  }
  if(R_EnvironmentIsLocked(env)) R_LockEnvironment(out, FALSE);
  return out;
}
