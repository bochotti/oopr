// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include <R.h>
#include <Rinternals.h>
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
extern "C" SEXP isname(SEXP x, SEXP names)
{
  if(!(TYPEOF(x) == SYMSXP && TYPEOF(names) == STRSXP))
  {
    return Rf_ScalarLogical(0);
  }
  R_xlen_t n = Rf_xlength(names);
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
extern "C" SEXP iscall(SEXP x, SEXP names)
{
  if(TYPEOF(x) != LANGSXP)
  {
    return Rf_ScalarLogical(0);
  }
  return isname(CAR(x), names);
}
