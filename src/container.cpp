// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "container.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class OoprContainer
{
public:
  OoprContainer(SEXP ooprC, SEXP thiz, SEXP map)
  {
    if(!is_ooprC(ooprC))        Rf_error("`ooprC` must be an ooprC object");
    if(!Rf_isEnvironment(thiz)) Rf_error("`thiz` not an environment");
    if(!Rf_isLogical(map))      Rf_error("`map` must be logical");
    args_ = R_ClosureFormals(ooprC);
    thiz_ = thiz;
    fun_  = R_getVar(sym["emplace"], thiz, FALSE);
    map_  = LOGICAL_ELT(map, 0);
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP makeArgs()
  {
    pSEXP a= map_ ? R_MissingArg : Rf_lang3(sym["$"], sym["this"], sym["size"]);
    pSEXP args = Rf_cons(a, args_);
    SET_TAG(args, sym["."]);
    return args;
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP makeBody()
  {
    const R_xlen_t len = Rf_xlength(args_);
    pSEXP sub = Rf_allocVector(VECSXP, len);
    R_xlen_t i = 0;
    for(SEXP e = args_; e != R_NilValue; e = CDR(e), ++i)
    {
      SET_VECTOR_ELT(sub, i, TAG(e));
    }
    pSEXP env = R_NewEnv(R_EmptyEnv, 1, 1);
    Rf_defineVar(sym["args"], sub, env);
    pSEXP expr = Rf_lang3(sym["substitute"], R_ClosureExpr(fun_), env);
    return Rf_eval(expr, R_BaseEnv);
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP replace()
  {
    pSEXP fun = R_mkClosure(makeArgs(), makeBody(), R_ClosureEnv(fun_));
    bool lock = R_BindingIsLocked(sym["emplace"], thiz_);
    R_unLockBinding(sym["emplace"], thiz_);
    Rf_defineVar(sym["emplace"], fun, thiz_);
    if(lock) R_LockBinding(sym["emplace"], thiz_);
    return Rf_ScalarLogical(1);
  }

private:
  static inline Symbols sym{
    "emplace", "$", "this", "size", ".", "args", "substitute"
  };
  SEXP args_;
  SEXP thiz_;
  SEXP fun_;
  bool map_;
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP oopr_cont_init(SEXP ooprC, SEXP thiz, SEXP map)
{
  OoprContainer obj(ooprC, thiz, map);
  return obj.replace();
}
