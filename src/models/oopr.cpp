// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "oopr.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprC::is(const RObj<SEXP, ALLSXP> gen, const RChr<SEXP> name)
{
  if(!(Rf_isS4(*gen) && gen.type() == CLOSXP && gen.inhr("ooprC"))) return false;

  const RObj<SEXP, ALLSXP> nme(gen.attr("name"));
  if(!(nme.type() == STRSXP && nme.size() == 1))                   return false;

  if(gen.attr("inhr").type() != STRSXP)                            return false;

  if(!OoprMeta::is(gen.attr("meta")))                              return false;

  const RObj<SEXP, ALLSXP> encl(gen.attr("encl"));
  if(encl.type() != ENVSXP)                                        return false;
  if(!Oopr::is(REnv<SEXP>(*encl)[".this"], name))                   return false;

  if(name.size() == 0) return true;
  const RStr<SEXP> n{RChr<SEXP>(nme)[0]};
  for(const RStr<SEXP>& nm : name) { if(nm == n) return true; }
  return false;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
OoprC::OoprC(const RObj<SEXP, ALLSXP> gen, const bool check)
  : RObj(check ? (is(gen) ? gen : (stop("Not an OoprC"), gen)) : gen)
  , name(gen.attr("name"))
  , inhr(gen.attr("inhr"))
  , meta(gen.attr("meta"), false)
  , encl(gen.attr("encl"))
  , thiz(encl["this"])
  , oopr(encl[".this"].get0(), false)
{ }


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Additional checks above checking S4 and class. If for some reason the
 * structure changes, cpp could crash R... so erring on side of caution.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool is_ooprC(SEXP obj, const std::string& name)
{
  // const RChr<PSEXP> nm = (name.size()) ? name : RChr<PSEXP>(0);
  // return OoprC::is(obj, nm);
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
bool Oopr::is(const RObj<SEXP, ALLSXP> obj, const RChr<SEXP> name)
{
  if(!(obj.type() == ENVSXP && obj.inhr("oopr")))        return false;
  const REnv<SEXP> intf(obj);
  const REnv<SEXP> inst(intf.parent());
  if(!(inst["this"].exists() && inst[".this"].exists())) return false;
  if(inst[".this"].get() != intf)                        return false;
  if(inst["this"].get().type() != ENVSXP)                return false;
  const REnv<SEXP> thiz(inst["this"].get());
  if(thiz.parent() != inst)                              return false;
  for(const RStr<SEXP>& nm : intf.names())
  {
    if(!thiz[nm].exists()) return false;
  }
  if(name.size() == 0) return true;
  for(const RStr<SEXP>& x : RChr<SEXP>(intf.cls()))
  {
    for(const RStr<SEXP>& y : name) { if (x == y) return true; }
  }
  return false;
}

Oopr::Oopr(const RObj<SEXP, ALLSXP> intf, const bool check)
  : REnv(check ? (is(intf) ? intf : (stop("Not an Oopr"), intf)) : intf)
  , encl(parent())
  , thiz(encl["this"])
{ }

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

