// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "breakpoint.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#define LIST(X)                                                \
  X(curl, "{")                                                 \
  X(iff, "if")                                                 \
  X(doTrace, ".doTrace")                                       \
  X(browser)                                                   \
  X(srcref)                                                    \
  X(orig)
SYMBOLS(LIST, sym)
#undef  LIST
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class Breakpoints
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  Breakpoints(
    const RObj<SEXP, CLOSXP> fun
   ,const RList<SEXP>        ats
   ,const RList<SEXP>        cond
  ) : body(R_ClosureExpr(*fun))
    , orig(this->body.attr(sym.orig))
    , ats(ats)
    , cond(cond)
  {
    if(CAR(*body) != *sym.curl)
    {
      stop("body of `fun` needs to be {");
    }
    if(ats.size() != cond.size())
    {
      stop("length of `ats` must == length of `cond`");
    }
    for(const RObj<SEXP, ALLSXP>& x : ats)
    {
      if(!RInt<>::is(*x)) { stop("all elements of `ats` must be integer"); }
    }
    switch(orig.type())
    {
    case LANGSXP:
    {
      SETCDR(*body, CDR(*orig));
      body.attr(sym.orig) = R_NilValue;
    }
    case NILSXP: break;
    default: orig = R_NilValue;
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  ~Breakpoints( )
  {
    if(!succ && orig != R_NilValue) { body.attr(sym.orig) = orig; }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void set( )
  {
    if(!ats.size())
    {
      succ = true;
      return;
    }
    body.attr(sym.orig) = LCONS(*sym.curl, Rf_duplicate(CDR(*body)));
    for(const int i : order())
    {
      SEXP par = nthcdr2(body, ats[i]);
      RObj<PSEXP, ALLSXP> val = Rf_lang3(
        *sym.curl
       ,Rf_lang3(
          *sym.iff, *cond[i]
         ,Rf_lang2(*sym.doTrace, Rf_lang1(*sym.browser))
        )
       ,CAR(par)
      );
      SEXP src = find_src_ref(ats[i], body);
      val.attr(sym.srcref) = RList<PSEXP>{src, src, src};
      SETCAR(par, *val);
    }
    succ = true;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  RObj<SEXP, LANGSXP> body;
  RObj<SEXP, ALLSXP>  orig;
  const RList<SEXP>   ats;
  const RList<SEXP>   cond;
  bool                succ{false};

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Sort the longest at vectors to be set first. This way multiple positions
   * do not conflict with each other.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  std::vector<int> order( )
  {
    std::vector<int> idx(ats.size());
    std::iota(idx.begin(), idx.end(), 0);
    std::sort(idx.begin(), idx.end(), [&](int l, int r)
    {
      return ats[l].size() > ats[r].size();
    });
    return idx;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // Breakpoints
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP set_breakpoints(SEXP fun, SEXP ats, SEXP cond) try
{
  Breakpoints bp(fun, ats, cond);
  bp.set();
  return fun;
}
catchR
