// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "oopr.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#define LIST(X)                                                \
  X(name)                                                      \
  X(inhr)                                                      \
  X(meta)                                                      \
  X(encl)                                                      \
  X(thiz, "this")                                              \
  X(intf, ".this")
SYMBOLS(LIST, sym)
#undef  LIST
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Check structure of an ooprC
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool OoprC::is(const RObj<SEXP, ALLSXP> gen)
{
  if(!(Rf_isS4(*gen) && gen.type() == CLOSXP && gen.inhr("ooprC")))
  {
    return false;
  }
  const RObj<SEXP, ALLSXP> name(gen.attr(sym.name));
  if(!(name.type() == STRSXP && name.size() == 1))           { return false; }
  if(!RChr<>::is(*gen.attr(sym.inhr)))                       { return false; }
  if(!OoprMeta::is(gen.attr(sym.meta)))                      { return false; }
  if(!REnv<>::is(*gen.attr(sym.encl)))                       { return false; }
  const REnv<SEXP> encl(gen.attr(sym.encl));
  if(!Oopr::is(encl[sym.intf].get0(), name))                 { return false; }
  const RChr<SEXP> inhr(gen.attr(sym.inhr));
  for(const RStr<SEXP>& name : inhr)
  {
    const REnv<SEXP>::Bind bind = encl[name.sym()];
    if(!(bind.exists() && OoprC::is(bind.get0(), { name }))) { return false; }
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
  const RStr<SEXP> name2(RChr<SEXP>(gen.attr(sym.name))[0]);
  for(const RStr<SEXP>& nm : name) { if(nm == name2) return true; }
  return false;
}
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Object model for ooprC
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
OoprC::OoprC(const RObj<SEXP, ALLSXP> gen, const bool check)
  : RObj(check ? (is(gen) ? gen : (stop("Not an ooprC"), gen)) : gen)
  , name(gen.attr(sym.name))
  , inhr(gen.attr(sym.inhr))
  , meta(gen.attr(sym.meta), false)
  , encl(gen.attr(sym.encl))
  , thiz(encl[sym.thiz].get0())
  , oopr(encl[sym.intf].get0(), false)
{ }


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Check the structure of an oopr instance.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool Oopr::is(const RObj<SEXP, ALLSXP> obj)
{
  if(!(obj.type() == ENVSXP && obj.inhr("oopr")))        { return false; }
  const REnv<SEXP> intf(obj);
  const REnv<SEXP> inst(intf.parent());
  const REnv<SEXP>::Bind bthiz(inst[sym.thiz])
                       , bintf(inst[sym.intf])
                       ;
  if(!(bthiz.exists() && bintf.exists()))                { return false; }
  if(bintf.get0() != intf)                               { return false; }
  if(bthiz.get0().type() != ENVSXP)                      { return false; }
  const REnv<SEXP> thiz(bthiz.get0());
  if(thiz.parent() != inst)                              { return false; }
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
  : REnv(check ? (is(intf) ? intf : (stop("Not an oopr"), intf)) : intf)
  , encl(parent())
  , thiz(encl[sym.thiz].get0())
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
