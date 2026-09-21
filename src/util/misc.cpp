// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "misc.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP amend_plist(SEXP x, SEXP at, SEXP v) try
{
  if(!Rf_isPairList(x)) { stop("`x` must be a pairlist"); }
  if(!RInt<>::is(at))   { stop("`at` must be an integer vector"); }

  const RInt<SEXP> ii  = at;
  const R_xlen_t   len = ii.size();
  SEXP y = x;
  for(R_xlen_t i = 0; i < len; ++i)
  {
    const int a = ii[i] - 1;
    if(!(0 <= a || a < Rf_xlength(x))) { stop("at=%i is out of bounds", a); }
    y = Rf_nthcdr(y, a);
    if(i < (len - 1)) { y = CAR(y); }
  }
  SETCAR(y, v);
  return x;
}
catchR

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP sexp_ptr(SEXP x)
{
  char buf[32];
  std::snprintf(buf, sizeof(buf), "%p", static_cast<void*>(x));
  return Rf_mkString(buf);
}
