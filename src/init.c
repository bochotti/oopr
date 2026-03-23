#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP findMemberRefs(SEXP);
extern SEXP findSrcRef(SEXP, SEXP);
extern SEXP getMissingVars(SEXP, SEXP);
extern SEXP interface(SEXP, SEXP, SEXP, SEXP);
extern SEXP iscall(SEXP, SEXP);
extern SEXP isname(SEXP, SEXP);
extern SEXP on_load(SEXP, SEXP);
extern SEXP oopr_make(SEXP);
extern SEXP oopr_tidy(SEXP, SEXP, SEXP);
extern SEXP symlink(SEXP, SEXP, SEXP, SEXP);
extern SEXP oopr_vec_init(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"findMemberRefs", (DL_FUNC) &findMemberRefs, 1},
    {"findSrcRef",     (DL_FUNC) &findSrcRef,     2},
    {"getMissingVars", (DL_FUNC) &getMissingVars, 2},
    {"interface",      (DL_FUNC) &interface,      4},
    {"iscall",         (DL_FUNC) &iscall,         2},
    {"isname",         (DL_FUNC) &isname,         2},
    {"on_load",        (DL_FUNC) &on_load,        2},
    {"oopr_make",      (DL_FUNC) &oopr_make,      1},
    {"oopr_tidy",      (DL_FUNC) &oopr_tidy,      3},
    {"symlink",        (DL_FUNC) &symlink,        4},
    {"oopr_vec_init",  (DL_FUNC) &oopr_vec_init,  3},
    {NULL, NULL, 0}
};

void R_init_oopr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
