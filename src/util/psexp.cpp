// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "./util/psexp.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Constructor, destructor
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
PSEXP::PSEXP(const SEXP x) : tkn(store(x)) { }
PSEXP::~PSEXP( ) noexcept { release(tkn); }

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Can be moved. The object moved from's token then becomes NULL so
 * release() will be skipped on destruction.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
PSEXP::PSEXP(PSEXP&& x) noexcept : tkn(x.tkn) { x.tkn = R_NilValue; }
PSEXP& PSEXP::operator=(PSEXP&& x) noexcept
{
  if(this != &x)
  {
    release(tkn);
    tkn = x.tkn;
    x.tkn = R_NilValue;
  }
  return *this;
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * If not token not initialized, just return NULL
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
SEXP PSEXP::get( ) const noexcept { return empty() ? R_NilValue : TAG(tkn); }
PSEXP::operator SEXP( )  const noexcept { return get(); }
SEXP PSEXP::operator*( ) const noexcept { return get(); }

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Set the underlying SEXP
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
void PSEXP::set(const SEXP x)
{
  if(empty())
  {
    tkn = store(x);
  }
  else
  {
    SET_TAG(tkn, x);
  }
}
PSEXP& PSEXP::operator=(const SEXP x)
{
  if(x != get()) set(x);
  return *this;
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Check if the token has been initialized
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool PSEXP::empty() const noexcept { return tkn == R_NilValue; }
void PSEXP::release() { release(tkn); tkn = R_NilValue; }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Static stuff.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Lazily create a pairlist object and protect it, only happens once.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
SEXP PSEXP::init() noexcept
{
  SEXP out = Rf_cons(R_NilValue, Rf_cons(R_NilValue, R_NilValue));
  R_PreserveObject(out);
  return out;
}
SEXP PSEXP::root() noexcept
{
  static SEXP out = init(); // ensure only one protected pairlist exists
  return out;
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Store a SEXP in the protected pairlist object.
 * Inserts a new pairlist object at the top of the protected pairlist, which
 *   - Holds a pointer to the pairlist prior,
 *   - Holds a pointer to the parilist after, and
 *   - Sets the SEXP object as the TAG.
 * Returns the new pairlist, where the SEXP object can be returned using
 * the TAG function.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
SEXP PSEXP::store(SEXP x) noexcept
{
  if(x == R_NilValue) return x;
  PROTECT(x);
  SEXP head  = root();
  SEXP next  = CDR(head);
  SEXP token = PROTECT(Rf_cons(head, next));
  SET_TAG(token, x);
  SETCDR(head, token);
  SETCAR(next, token);
  UNPROTECT(2);
  return token;
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Remove the token from the protected pairlist.
 * Because the token holds pointers to the pairlists prior and after, those
 * can be amended to remove the pointer to the token.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
void PSEXP::release(SEXP token) noexcept
{
  if(token == R_NilValue) return; // uninitialized token is skipped
  SEXP lhs = CAR(token);
  SEXP rhs = CDR(token);
  SETCDR(lhs, rhs);
  SETCAR(rhs, lhs);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
R_xlen_t PSEXP::size( ) noexcept { return Rf_xlength(root()) - 2; }
SEXP     PSEXP::list( ) noexcept
{
  SEXP out  = PROTECT(R_NilValue);
  for(SEXP x = CDR(root()); CDR(x) != R_NilValue; x = CDR(x))
  {
    out = Rf_cons(TAG(x), out);
  }
  UNPROTECT(1);
  return out;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP get_PSEXPs()
{
  return PSEXP::list();
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
