#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP find_member_refs(SEXP);
extern SEXP find_src_ref    (SEXP, SEXP);
extern SEXP get_missing_vars(SEXP, SEXP);
extern SEXP interface       (SEXP, SEXP, SEXP, SEXP);
extern SEXP iscall          (SEXP, SEXP, SEXP);
extern SEXP isname          (SEXP, SEXP);
extern SEXP on_load         (SEXP, SEXP);
extern SEXP oopr_make       (SEXP, SEXP, SEXP);
extern SEXP symlinkR        (SEXP, SEXP, SEXP, SEXP);
extern SEXP oopr_cont_init  (SEXP, SEXP);
extern SEXP find_instances  (SEXP, SEXP, SEXP);
extern SEXP eval_context    (SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"find_member_refs", (DL_FUNC) &find_member_refs, 1},
    {"find_src_ref",     (DL_FUNC) &find_src_ref,     2},
    {"get_missing_vars", (DL_FUNC) &get_missing_vars, 2},
    {"interface",        (DL_FUNC) &interface,        4},
    {"iscall",           (DL_FUNC) &iscall,           3},
    {"isname",           (DL_FUNC) &isname,           2},
    {"on_load",          (DL_FUNC) &on_load,          2},
    {"oopr_make",        (DL_FUNC) &oopr_make,        3},
    {"symlink",          (DL_FUNC) &symlinkR,         4},
    {"oopr_cont_init",   (DL_FUNC) &oopr_cont_init,   3},
    {"find_instances",   (DL_FUNC) &find_instances,   3},
    {"eval_context",     (DL_FUNC) &eval_context,     3},
    {NULL, NULL, 0}
};

void R_init_oopr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
