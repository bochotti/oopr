// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef OOPR_UTIL_PSEXP_H
#define OOPR_UTIL_PSEXP_H
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "common.h"
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * A class which protects SEXP objects
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class pSEXP
{
public:
  pSEXP(SEXP x)              { load(x); }
  pSEXP()                    { }
  virtual ~pSEXP()           { unload(); }
  operator SEXP()            { return x; }
  operator SEXP() const      { return x; }
  pSEXP& operator=(SEXP x)   { load(x); return *this; }

  pSEXP(pSEXP&& other) noexcept { *this = std::move(other); }
  pSEXP& operator=(pSEXP&& other) noexcept
  {
    x = other.x;
    other.x = R_NilValue;
    return *this;
  }

protected:
  SEXP x = R_NilValue;
  void load(SEXP x)          { unload(); if(nNull(x)) prtct(x); this->x = x; }
  void unload()              { if(nNull(x)) unprtct(); x = R_NilValue; }
private:
  inline  bool nNull(SEXP x) { return x != R_NilValue; }
  virtual void prtct(SEXP x) { PROTECT(x);       }
  virtual void unprtct()     { UNPROTECT_PTR(x); }
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // pSEXP
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif /* OOPR_UTIL_PSEXP_H */
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
