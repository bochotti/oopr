// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef OOPR_BREAKPOINT_H
#define OOPR_BREAKPOINT_H
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "common.h"
#include <vector>
#include <numeric>
#include <algorithm>
#include "./util/unwind.h"
#include "./util/psexp.h"
#include "./util/symbols.h"
#include "./util/misc.h"
#include "./models/oopr.h"
#include "./models/meta.h"
#include "./reference.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
extern "C" SEXP set_breakpoints(SEXP fun, SEXP ats, SEXP cond);
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
extern "C" SEXP LCS(SEXP x, SEXP y);
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif
