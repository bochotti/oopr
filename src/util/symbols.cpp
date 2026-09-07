// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "symbols.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP isname(SEXP x, SEXP names)
{
  if(!(Rf_isSymbol(x) && Rf_isString(names)))
  {
    return Rf_ScalarLogical(0);
  }
  const R_xlen_t n = Rf_xlength(names);
  if(n == 0)
  {
    return Rf_ScalarLogical(1);
  }
  const char *name = CHAR(PRINTNAME(x));
  for(R_xlen_t i = 0; i < n; ++i)
  {
    if(strcmp(CHAR(STRING_ELT(names, i)), name) == 0)
    {
      return Rf_ScalarLogical(1);
    }
  }
  return Rf_ScalarLogical(0);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP iscall(SEXP x, SEXP names, SEXP package)
{
  if(!Rf_isLanguage(x))
  {
    return Rf_ScalarLogical(0);
  }
  if(Rf_xlength(package) == 0)
  {
    return isname(CAR(x), names);
  }

  Symbols sym{"::", ":::"};
  x = CAR(x);
  if(!sym.is(CAR(x))) return Rf_ScalarLogical(0);
  if(!LOGICAL_ELT(isname(CADR(x), package), 0)) return Rf_ScalarLogical(0);
  return isname(CADDR(x), names);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Symbols
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
Symbols::Symbols(std::initializer_list<std::string> syms)
{
  syms_.reserve(syms.size());
  for(const std::string& sym : syms)
  {
    syms_.emplace(sym, Rf_install(sym.c_str()));
  }
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool Symbols::is(SEXP x) const
{
  if(!Rf_isSymbol(x)) return false;
  for(const auto& sym : syms_) { if(x == sym.second) return true; }
  return false;
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool Symbols::is(SEXP x, const std::initializer_list<std::string>& keys) const
{
  if(!Rf_isSymbol(x)) return false;
  for(const std::string& key : keys) if(x == get(key)) return true;
  return false;
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool Symbols::is(SEXP x, const std::string& key) const
{
  if(!Rf_isSymbol(x)) return false;
  return x == get(key);
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP Symbols::get(const std::string& key) const
{
  auto it = syms_.find(key);
  if(it == syms_.end()) { stop("`%s` not a key", key.c_str()); }
  return it->second;
}
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Symbols
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
