// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "oopr.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Check structure of an ooprC
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool OoprC::is(const RObj<SEXP, ALLSXP> gen)
{
  if(!(Rf_isS4(*gen) && gen.type()==CLOSXP && gen.inhr("ooprC")))  return false;
  const RObj<SEXP, ALLSXP> name(gen.attr("name"));
  if(!(name.type() == STRSXP && name.size() == 1))                 return false;
  if(!RChr<>::is(*gen.attr("inhr")))                               return false;
  if(!OoprMeta::is(gen.attr("meta")))                              return false;
  if(!REnv<>::is(*gen.attr("encl")))                               return false;
  const REnv<SEXP> encl(gen.attr("encl"));
  if(!Oopr::is(encl[".this"], name))                               return false;
  const RChr<SEXP> inhr(gen.attr("inhr"));
  for(const RStr<SEXP>& name : inhr)
  {
    REnv<SEXP>::Bind bind = encl[name.sym()];
    if(!(bind.exists() && OoprC::is(bind, { name })))             return false;
  }
  return true;
}
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Check class name of an ooprC
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool OoprC::is(const RObj<SEXP, ALLSXP> gen, const RChr<SEXP> name)
{
  if(!is(gen))         return false;
  if(name.size() == 0) return true;
  const RStr<SEXP> name2(RChr<SEXP>(gen.attr("name"))[0]);
  for(const RStr<SEXP>& nm : name) { if(nm == name2) return true; }
  return false;
}
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Object model for ooprC
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
OoprC::OoprC(const RObj<SEXP, ALLSXP> gen, const bool check)
  : RObj(check ? (is(gen) ? gen : (stop("Not an OoprC"), gen)) : gen)
  , name(gen.attr("name"))
  , inhr(gen.attr("inhr"))
  , meta(gen.attr("meta"), false)
  , encl(gen.attr("encl"))
  , thiz(encl["this"])
  , oopr(encl[".this"].get0(), false)
{ }


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Check the structure of an oopr instance.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool Oopr::is(const RObj<SEXP, ALLSXP> obj)
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
  return true;
}
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Check class of oopr instance
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool Oopr::is(const RObj<SEXP, ALLSXP> obj, const RChr<SEXP> name)
{
  if(!is(obj)) return false;
  const REnv<SEXP> intf(obj);
  if(name.size() == 0) return true;
  for(const RStr<SEXP>& x : RChr<SEXP>(intf.cls()))
  {
    for(const RStr<SEXP>& y : name) { if (x == y) return true; }
  }
  return false;
}
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Object model for oopr instance
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
Oopr::Oopr(const RObj<SEXP, ALLSXP> intf, const bool check)
  : REnv(check ? (is(intf) ? intf : (stop("Not an Oopr"), intf)) : intf)
  , encl(parent())
  , thiz(encl["this"])
{ }


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Exported to R
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
SEXP isooprC(SEXP obj, SEXP name) try
{
  return OoprC::is(obj, name) ? Rf_ScalarLogical(1) : Rf_ScalarLogical(0);
}
catchR


SEXP isoopr(SEXP obj, SEXP name) try
{
  return Oopr::is(obj, name) ? Rf_ScalarLogical(1) : Rf_ScalarLogical(0);
}
catchR
