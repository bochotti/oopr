// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "breakpoint.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#define LIST(X)                                                \
  X(curl, "{")                                                 \
  X(iff, "if")                                                 \
  X(isTRUE)                                                    \
  X(tri, "try")                                                \
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
    RObj<PSEXP> orig = LCONS(*sym.curl, Rf_duplicate(CDR(*body)));
    orig.attr(sym.srcref) = body.attr(sym.srcref);
    body.attr(sym.orig) = orig;
    for(const int i : order())
    {
      SEXP part = nthcdr2(body, ats[i]);
      SEXP src  = find_src_ref(ats[i], body);
      RObj<PSEXP, ALLSXP> expr = prefixBreakpoint(part, cond[i]);
      expr.attr(sym.srcref)    = RList<PSEXP>{src, src, src};
      SETCAR(part, *expr);
    }
    succ = true;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  RObj<SEXP, LANGSXP> body;
  RObj<PSEXP, ALLSXP> orig;
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

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Creates the expression that calls the browser, e.g.:
   * {
   *   if(isTRUE(cond)) .doTrace(browser());
   *   expr;
   * }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  RObj<PSEXP, LANGSXP> prefixBreakpoint(SEXP expr, const RObj<SEXP>& cond)
  {
    RObj<PSEXP, ALLSXP> browser = Rf_lang3(
      *sym.iff
     ,Rf_lang2(*sym.isTRUE, Rf_lang3(*sym.tri, *cond, Rf_ScalarLogical(1)))
     ,Rf_lang2(*sym.doTrace, Rf_lang1(*sym.browser))
    );
    return Rf_lang3(*sym.curl, *browser, CAR(expr));
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


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class Matrix2D
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  Matrix2D(int nrow, int ncol, int init = 0)
    : data_(nrow * ncol, init)
    , nrow_(nrow)
    , ncol_(ncol)
  { }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  const std::vector<int>& data( ) const noexcept { return data_; }
  int                     nrow( ) const noexcept { return nrow_; }
  int                     ncol( ) const noexcept { return ncol_; }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  class Row
  {
  friend class Matrix2D;
  public:
    int& operator[](const int c) { return mtx.data_[(r * mtx.ncol()) + c]; }
  private:
    Row(Matrix2D& mtx, const int r) : mtx(mtx), r(r) { }
    Matrix2D& mtx;
    const int r;
  };
  Row operator[](const int r) { return {*this, r}; }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  std::vector<int> data_;
  int              nrow_;
  int              ncol_;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // Matrix2D
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP LCS(SEXP x, SEXP y) try
{
  const RChr<SEXP> X = x; const R_xlen_t nrow = X.size();
  const RChr<SEXP> Y = y; const R_xlen_t ncol = Y.size();

  Matrix2D M(nrow + 1, ncol + 1, 0);
  for(int i = nrow - 1; i >= 0; --i) for(int j = ncol - 1; j >= 0; --j)
  {
    if(X[i] == Y[j])
    {
      M[i][j] = M[i + 1][j + 1] + 1;
    }
    else
    {
      M[i][j] = std::max(M[i][j + 1], M[i + 1][j]);
    }
  }

  std::vector<int> out(nrow, NA_INTEGER);
  int i = 0, j = 0;
  while(i < nrow && j < ncol)
  {
    if(X[i] == Y[j])
    {
      out[i] = j + 1;
      ++i; ++j;
    }
    else if(M[i + 1][j] >= M[i][j + 1])
    {
      ++i;
    }
    else
    {
      ++j;
    }
  }
  return *RInt<PSEXP>(out);
}
catchR
