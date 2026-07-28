// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "common.h"
#if R_VERSION < R_Version(4, 5, 0)
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP R_mkClosure(SEXP formals, SEXP body, SEXP env)
{
    SEXP fun = Rf_allocSExp(CLOSXP);
    SET_FORMALS(fun, formals);
    SET_BODY(fun, body);
    SET_CLOENV(fun, env);
    return fun;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP R_getVar(SEXP sym, SEXP rho, Rboolean inherits)
{
  SEXP val = R_getVarEx(sym, rho, inherits, R_UnboundValue);
  if (val == R_UnboundValue)
    Rf_error("object '%s' not found", Rf_translateChar(PRINTNAME(sym)));
  return val;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP R_getVarEx(SEXP sym, SEXP rho, Rboolean inherits, SEXP ifnotfound)
{
  SEXP val = inherits ? Rf_findVar(sym, rho) : Rf_findVarInFrame(rho, sym);
  if(val == R_UnboundValue) return ifnotfound;
  if(TYPEOF(val) == PROMSXP)
  {
    PROTECT(val);
    val = Rf_eval(val, rho);
    UNPROTECT(1);
  }
  return val;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
