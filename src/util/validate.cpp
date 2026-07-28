// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "validate.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Additional checks above checking S4 and class. If for some reason the
 * structure changes, cpp could crash R... so erring on side of caution.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool is_ooprC(SEXP obj, const std::string& name)
{
  if(!(Rf_isS4(obj) && Rf_inherits(obj, "ooprC")))                 return false;

  SEXP nm = Rf_getAttrib(obj, Rf_install("name"));
  if(!(Rf_isString(nm) && Rf_xlength(nm) == 1))                    return false;
  if(!name.empty())
  {
    if(strcmp(name.c_str(), R_CHAR(STRING_ELT(nm, 0))) != 0)       return false;
  }

  SEXP inhr = Rf_getAttrib(obj, Rf_install("inhr"));
  if(!Rf_isString(inhr))                                           return false;

  SEXP meta = Rf_getAttrib(obj, Rf_install("meta"));
  if(!(Rf_isEnvironment(meta) && Rf_inherits(meta, "oopr_meta")))  return false;

  SEXP encl = Rf_getAttrib(obj, Rf_install("encl"));
  if(!Rf_isEnvironment(encl))                                      return false;

  if(!is_oopr(R_getVarEx(Rf_install(".this"), encl, FALSE, R_NilValue), name))
    return false;

  return true;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool is_ooprC(SEXP obj, SEXP name)
{
  std::string name2;
  if(Rf_isSymbol(name))
  {
    name2 = R_CHAR(PRINTNAME(name));
  }
  else if(Rf_isString(name) && Rf_length(name) > 0)
  {
    name2 = R_CHAR(STRING_ELT(name, 0));
  }
  else
  {
    return false;
  }
  return is_ooprC(obj, name2);
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Check the structure of an oopr instance.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool is_oopr(SEXP obj, const std::string& name)
{
  if(!(Rf_isEnvironment(obj) && Rf_inherits(obj, "oopr"))) return false;
  if(!name.empty() && !Rf_inherits(obj, name.c_str()))     return false;

  // the enclosure should have both this and .this
  SEXP encl = R_ParentEnv(obj);
  SEXP thiz = Rf_install("this");
  SEXP intf = Rf_install(".this");
  if(!R_existsVarInFrame(encl, thiz))                      return false;
  if(!R_existsVarInFrame(encl, intf))                      return false;

  // this and .this should have encl as their parent
  thiz = R_getVar(thiz, encl, FALSE);
  intf = R_getVar(intf, encl, FALSE);
  if(obj != intf || encl != R_ParentEnv(thiz))             return false;

  // all bindings inside .this should also be in this
  SEXP names = R_lsInternal3(intf, TRUE, FALSE);
  const R_xlen_t len = Rf_xlength(names);
  for(R_xlen_t i = 0; i < len; ++i)
  {
    SEXP name = Rf_installChar(STRING_ELT(names, i));
    if(!R_existsVarInFrame(thiz, name))                    return false;
  }
  return true;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool is_oopr(SEXP obj, SEXP name)
{
  std::string name2;
  if(Rf_isSymbol(name))
  {
    name2 = R_CHAR(PRINTNAME(name));
  }
  else if(Rf_isString(name) && Rf_length(name) > 0)
  {
    name2 = R_CHAR(STRING_ELT(name, 0));
  }
  else
  {
    return false;
  }
  return is_oopr(obj, name2);
}

