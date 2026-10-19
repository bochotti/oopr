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
const SEXP* VECTOR_PTR_RO(SEXP x)
{
  if(TYPEOF(x) != VECSXP)
  Rf_error("%s() can only be applied to a '%s', not a '%s'",
        __func__, "list", Rf_type2char(TYPEOF(x)));
  return reinterpret_cast<const SEXP*>(DATAPTR_RO(x));
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#if R_VERSION < R_Version(4, 3, 0)
SEXP R_ParseString(const char* str)
{
  SEXP s = PROTECT(Rf_mkString(str));
  ParseStatus status;
  SEXP ps = PROTECT(R_ParseVector(s, -1, &status, R_NilValue));
  if (status != PARSE_OK ||
    TYPEOF(ps) != EXPRSXP ||
    LENGTH(ps) != 1)
    Rf_error("parse error");
  SEXP val = VECTOR_ELT(ps, 0);
  UNPROTECT(2); /* s, ps */
  return val;
}
#endif
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
