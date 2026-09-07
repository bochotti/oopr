// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef OOPR_ENCLOSURE_H
#define OOPR_ENCLOSURE_H
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "common.h"
#include "./util/unwind.h"
#include "./util/psexp.h"
#include "./util/symlink.h"
#include "./models/robj.h"
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Creates an interface environment. Which holds references back to the
 * original environment. It will copy methods and active bindings directly,
 * otherwise it creates an active binding referring back to the original.
 * Can provide names and class, the defaults copy `env`.
 * Locking bindings are carried over, as is the locked status of `env`.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
extern "C" SEXP interface(SEXP env, SEXP nme, SEXP nms, SEXP cls);
SEXP interface(
  SEXP env              // The environment to refer to
 ,SEXP nme              // The name of `env` in its parent environment
 ,SEXP nms = R_NilValue // Names from environment to refer to
 ,SEXP cls = R_NilValue // Class to assign to the output
 ,bool chk = true       // Check symlinkR assumptions
);
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif
