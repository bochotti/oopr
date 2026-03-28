// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "utils.h"
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
SEXP symlink(SEXP tenv, SEXP tname, SEXP env, SEXP name, bool check)
{
  if(!Rf_isEnvironment(tenv)) Rf_error("`tenv` must be an environment");
  if(!Rf_isEnvironment(env))  Rf_error("`env` must be an environment");

  if(!Rf_isSymbol(tname))
  {
    if(!(Rf_isString(tname) && Rf_xlength(tname) == 1L))
    {
      Rf_error("`tname` must be a symbol or single character vector");
    }
    tname = Rf_installChar(STRING_ELT(tname, 0));
  }
  if(!R_existsVarInFrame(ENCLOS(tenv), tname))
  {
    Rf_error("`tname` does not exist in the parent environment of `tenv`");
  }

  if(!Rf_isSymbol(name))
  {
    if(!(Rf_isString(name) && Rf_xlength(name) == 1L))
    {
      Rf_error("`name` must be a symbol or single character vector");
    }
    name = Rf_installChar(STRING_ELT(name, 0));
  }
  if(check && !R_existsVarInFrame(tenv, name))
  {
    Rf_error("`name` does not exist in `tenv`");
  }
  if(check && R_existsVarInFrame(env, name))
  {
    Rf_error("`name` already exists in `env`");
  }

  pSEXP x   = Rf_install("x");
  pSEXP fun = Rf_allocSExp(CLOSXP);
  pSEXP arg = Rf_allocList(1); SET_TAG(arg, x); SETCAR(arg, R_MissingArg);
  pSEXP bdy = Rf_lang4(
    Rf_install("if"), Rf_lang2(Rf_install("missing"), x)
   ,Rf_lang3(Rf_install("$"), tname, name)
   ,Rf_lang3(Rf_install("<-"), Rf_lang3(Rf_install("$"), tname, name), x)
  );
  SET_FORMALS(fun, arg);
  SET_BODY(fun, bdy);
  SET_CLOENV(fun, ENCLOS(tenv));
  R_MakeActiveBinding(name, fun, env);
  return Rf_ScalarLogical(1);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
Symbols::Symbols(std::initializer_list<std::string> syms)
{
  for(const std::string& sym : syms)
  {
    syms_.emplace(sym, Rf_install(sym.c_str()));
  }
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool Symbols::is(SEXP x)
{
  if(!Rf_isSymbol(x)) return false;
  for(const auto& [key, val] : syms_) if(x == val) return true;
  return false;
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool Symbols::is(SEXP x, const std::initializer_list<std::string>& keys)
{
  if(!Rf_isSymbol(x)) return false;
  for(const std::string& key : keys) if(x == get(key)) return true;
  return false;
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool Symbols::is(SEXP x, const std::string& key)
{
  if(!Rf_isSymbol(x)) return false;
  return x == get(key);
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP Symbols::get(const std::string& key)
{
  if(syms_.find(key) == syms_.end()) Rf_error("`%s` not a key", key.c_str());
  return syms_[key];
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
std::map<std::string, SEXP> Symbols::syms() { return syms_; };
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Additional checks above checking S4 and class. If for some reason the
 * structure changes, cpp could crash R... so erring on side of caution.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool is_ooprC(SEXP obj, const std::string& name)
{
  if(!(Rf_isS4(obj) && Rf_inherits(obj, "ooprC"))) return false;

  SEXP nm = Rf_getAttrib(obj, Rf_install("name"));
  if(!(Rf_isString(nm) && Rf_xlength(nm) == 1)) return false;
  if(!name.empty())
  {
    if(strcmp(name.c_str(), CHAR(STRING_ELT(nm, 0))) != 0) return false;
  }

  SEXP inhr = Rf_getAttrib(obj, Rf_install("inhr"));
  if(!Rf_isString(inhr)) return false;

  SEXP meta = Rf_getAttrib(obj, Rf_install("meta"));
  if(!(Rf_isEnvironment(meta) && Rf_inherits(meta, "oopr_meta"))) return false;

  SEXP encl = Rf_getAttrib(obj, Rf_install("encl"));
  if(!Rf_isEnvironment(encl)) return false;

  return true;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool is_ooprC(SEXP obj, SEXP name)
{
  std::string name2;
  if(Rf_isSymbol(name))
  {
    name2 = CHAR(PRINTNAME(name));
  }
  else if(Rf_isString(name) && Rf_length(name) > 0)
  {
    name2 = CHAR(STRING_ELT(name, 0));
  }
  else
  {
    return false;
  }
  return is_ooprC(obj, name2);
}
