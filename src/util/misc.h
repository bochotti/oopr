// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "../common.h"
#include "unwind.h"
#include "../models/robj.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP nthcdr2(const RObj<SEXP, LANGSXP> x, const RInt<SEXP> at);

extern "C" SEXP amend_plist(SEXP x, SEXP at, SEXP v);

extern "C" SEXP sexp_ptr(SEXP x);
