// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "container.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class OoprContainer
{
public:
  OoprContainer(const OoprC gen, REnv<SEXP> thiz, const RLgl<SEXP> map)
    : args_(R_ClosureFormals(*gen))
    , bind_(thiz[sym["emplace"]])
    , map_(map[0])
  {
    if(!bind_.exists()) stop("`emplace` is a required binding for thiz");
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP replace()
  {
    PSEXP fun = R_mkClosure(makeArgs(), makeBody(), R_ClosureEnv(*bind_));
    const bool lock = bind_.locked();
    bind_.lock(false);
    bind_.assign(fun);
    bind_.lock(lock);
    return Rf_ScalarLogical(1);
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  static const Symbols sym;
  const SEXP           args_;
  REnv<SEXP>::Bind     bind_;
  const bool           map_;

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP makeArgs()
  {
    PSEXP args = Rf_cons(
      map_ ? R_MissingArg : Rf_lang3(sym["$"], sym["this"], sym["size"])
     ,args_
    );
    SET_TAG(args, sym["."]);
    return args;
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP makeBody()
  {
    const R_xlen_t len = Rf_xlength(args_);
    RList<PSEXP> sub(len);
    R_xlen_t i = 0;
    for(SEXP e = args_; e != R_NilValue; e = CDR(e), ++i) { sub[i] = TAG(e); }

    REnv<PSEXP> env(REnv<SEXP>(R_EmptyEnv), false, 1);
    env[sym["args"]] = sub;

    PSEXP expr = Rf_lang3(sym["substitute"], R_ClosureExpr(*bind_), *env);
    return RUnWind::eval(expr, R_BaseEnv);
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // OoprContainer
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
const Symbols OoprContainer::sym{
  "emplace", "$", "this", "size", ".", "args", "substitute"
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP oopr_cont_init(SEXP ooprC, SEXP thiz, SEXP map) try
{
  if(!OoprC::is(ooprC)) stop("`ooprC` must be an ooprC object");
  if(!REnv<>::is(thiz)) stop("`thiz` not an environment");
  if(!RLgl<>::is(map))  stop("`map` must be logical");
  return OoprContainer(OoprC(ooprC, false), thiz, map).replace();
}
catchR
