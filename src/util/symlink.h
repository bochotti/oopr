// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef OOPR_UTIL_SYMLINK_H
#define OOPR_UTIL_SYMLINK_H
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "../common.h"
#include "unwind.h"
#include "psexp.h"
#include "symbols.h"
#include "../models/robj.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
extern "C" SEXP symlinkR(SEXP tenv, SEXP tname, SEXP env, SEXP name);
SEXP symlinkR(SEXP tenv, SEXP tname, SEXP env, SEXP name, bool check = true);
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif /* OOPR_UTIL_SYMLINK_H */
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
