// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "install.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#define LIST(X)                                                \
  X(thiz, "this")                                              \
  X(intf, ".this")                                             \
  X(curl, "{")
SYMBOLS(LIST, sym)
#undef  LIST
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class OoprLoad
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  OoprLoad(const REnv<SEXP> env, REnv<SEXP> ns) : env(env), ns(ns) { }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadEnv()
  {
    for(const RStr<SEXP>& name : env.names())
    {
      const RSym sym(name.sym());
      SEXP x = *env[sym];
      if(!OoprC::is(x, { name })) { continue; }
      OoprC ooprC(x, false);
      loadOopr(ooprC);
      ns[sym] = x;
    }
  }
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  const REnv<SEXP> env;
  REnv<SEXP>       ns;

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadOopr(OoprC& ooprC)
  {
    for(const RStr<SEXP>& name : ooprC.inhr)
    {
      const RSym sym(name.sym());
      const OoprC ooprI(ooprC.encl[name]);
      loadInhr(sym, ooprI, ooprC);
    }

    const OoprMeta& meta = ooprC.meta;
    REnv<SEXP>      thiz = ooprC.encl[sym.thiz];
    for(R_xlen_t i = 0; i < meta.size(); ++i)
    {
      if(!meta.isClass(i) || meta.isInherit(i)) continue;
      const RSym name(meta.name(i));
      REnv<SEXP>::Bind bind = thiz[name];
      if(meta.isStatic(i))
      {
        loadStaticClass(bind);
      }
      else if(OoprC::is(*bind))
      {
        const OoprC ooprM(OoprC(bind, false));
        const OoprC ooprN = fromAnotherPackage(ooprM);
        if(ooprM != ooprN)
        {
          setLockedBinding(thiz[name], ooprN);
        }
      }
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadInhr(const RSym name, const OoprC& ooprI, OoprC& ooprC)
  {
    const OoprC ooprN = fromAnotherPackage(ooprI);
    if(ooprI == ooprN) { return; }
    const REnv<SEXP>& enclI = ooprN.encl;
    REnv<SEXP>         encl = ooprC.encl;
    const OoprMeta&    meta = ooprC.meta;
    setLockedBinding(encl[name], ooprN);
    for(R_xlen_t i = 0; i < meta.size(); ++i)
    {
      if(meta.inherit(i) != name) continue;
      loadMember(i, meta, encl, enclI);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadStaticClass(const REnv<SEXP>::Bind& bind)
  {
    if(!Oopr::is(*bind)) { return; }
    const Oopr oopr(*bind, false);
    const RSym cls(oopr.cls()[0].sym());
    const RObj<SEXP, ALLSXP> x = oopr.topenv()[cls].get0();
    if(!OoprC::is(x, { cls.chr() })) { return; }
    const OoprC ooprM(x, false);
    const OoprMeta& meta = ooprM.meta;
    for(R_xlen_t i = 0; i < meta.size(); ++i)
    {
      if(!meta.isStatic(i)) { continue; }
      loadMember(i, meta, oopr.encl, ooprM.encl);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  OoprC fromAnotherPackage(const OoprC& ooprC)
  {
    const REnv<SEXP> top(ooprC.encl.topenv());
    if(!(R_IsNamespaceEnv(*top) && top != ns)) { return ooprC; }
    const REnv<SEXP>::Bind bind(top[ooprC.name[0].sym()]);
    if(!OoprC::is(bind, ooprC.name))           { return ooprC; }
    return OoprC(bind, false);
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadMember(
    const R_xlen_t i, const OoprMeta& meta
   ,REnv<SEXP> encl,  const REnv<SEXP> enclI
  )
  {
    const RSym          name  = meta.name(i);
    REnv<SEXP>          thiz  = encl[sym.thiz];
    REnv<SEXP>::Bind    bind  = thiz[name];
    RObj<PSEXP, CLOSXP> fun   = meta.isMethod(i) ? *bind : *bind.fun;

    fun = R_mkClosure(R_ClosureFormals(*fun), R_ClosureExpr(*fun), *enclI);
    setLockedBinding(bind, fun);
    if(meta.isStatic(i) && meta.isAccess(i, "public"))
    {
      thiz = encl[sym.intf];
      setLockedBinding(thiz[name], fun);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void setLockedBinding(REnv<SEXP>::Bind bind, const RObj<SEXP, ALLSXP> value)
  {
    const bool lock = bind.locked();
    bind.lock(false);
    if(bind.active() && value.type() == CLOSXP)
    {
      bind.fun = value;
    }
    else
    {
      bind = value;
    }
    bind.lock(lock);
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // OoprLoad
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP on_load(SEXP env, SEXP ns) try
{
  if(!(REnv<>::is(env) && REnv<>::is(ns))) return Rf_ScalarLogical(0);
  if(!R_IsNamespaceEnv(ns))                return Rf_ScalarLogical(0);
  OoprLoad obj(env, ns);
  obj.loadEnv();
  return Rf_ScalarLogical(1);
}
catchR
