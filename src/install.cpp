// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "install.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#define LIST(X)                                                \
  X(thiz, "this")                                              \
  X(intf, ".this")
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
    const RChr<PSEXP> names = env.names();
    // put classes back into ns, so they can be found in topenv.
    for(const RStr<SEXP>& name : names)
    {
      const RSym sym(name.sym());
      ns[sym] = env[sym].get0();
    }
    for(const RStr<SEXP>& name : names)
    {
      const RSym sym(name.sym());
      RObj<SEXP> x(env[sym].get0());
      if(!OoprC::is(x, { name })) { continue; }
      OoprC ooprC(x, false);
      loadOopr(ooprC);
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
      const OoprC ooprI(ooprC.encl[name].get0(), false);
      loadInhr(sym, ooprI, ooprC);
    }

    const OoprMeta& meta = ooprC.meta;
    REnv<SEXP>      thiz = ooprC.thiz;
    REnv<SEXP>      intf = ooprC.oopr;
    for(R_xlen_t i = 0; i < meta.size(); ++i)
    {
      const RSym name(meta.name(i));
      // de-dupe static methods encl$this & encl$.this
      if(meta.isAccess(i, "public") && meta.isStatic(i))
      {
        if(meta.isMethod(i))
        {
          setLockedBinding(intf[name], thiz[name]);
        }
        else if(thiz[name].active())
        {
          setLockedBinding(intf[name], thiz[name].fun);
        }
      }
      if(!meta.isClass(i) || meta.isInherit(i)) { continue; }
      RObj<SEXP> x(thiz[name].get0());
      if(meta.isStatic(i) && Oopr::is(x))
      {
        const Oopr oopr(x, false);
        loadStaticClass(oopr);
      }
      else if(OoprC::is(x))
      {
        const OoprC ooprM(x, false);
        const OoprC ooprN = getFromTop(ooprM);
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
    const OoprC       ooprN = getFromTop(ooprI);
    const REnv<SEXP>& enclI = ooprN.encl;
    REnv<SEXP>        encl  = ooprC.encl;
    const OoprMeta&   meta  = ooprC.meta;
    if(ooprI != ooprN)
    {
      setLockedBinding(encl[name], ooprN);
    }
    for(R_xlen_t i = 0; i < meta.size(); ++i)
    {
      if(meta.inherit(i) != name) { continue; }
      loadMember(i, meta, encl, enclI);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadStaticClass(const Oopr& oopr)
  {
    const RSym       cls(oopr.cls()[0].sym());
    const RObj<SEXP> x(oopr.topenv()[cls].get0());
    if(!OoprC::is(x, { cls.chr() })) { return; }
    const OoprC ooprM(x, false);
    const OoprMeta&  meta(ooprM.meta);
    for(R_xlen_t i = 0; i < meta.size(); ++i)
    {
      if(!meta.isStatic(i)) { continue; }
      loadMember(i, meta, oopr.encl, ooprM.encl);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  OoprC getFromTop(const OoprC& ooprC)
  {
    const REnv<SEXP> top(ooprC.encl.topenv());
    if(!(R_IsNamespaceEnv(*top)))              { return ooprC; }
    const REnv<SEXP>::Bind bind(top[ooprC.name[0].sym()]);
    if(!OoprC::is(bind.get0(), ooprC.name))    { return ooprC; }
    return OoprC(bind.get0(), false);
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadMember(
    const R_xlen_t i, const OoprMeta& meta
   ,REnv<SEXP> encl,  const REnv<SEXP> enclI
  )
  {
    const RSym          name  = meta.name(i);
    REnv<SEXP>          thiz  = encl[sym.thiz].get0();
    REnv<SEXP>          thizI = enclI[sym.thiz].get0();
    REnv<SEXP>::Bind    bind  = thiz[name];
    REnv<SEXP>::Bind    bindI = thizI[name];
    RObj<PSEXP, ALLSXP> fun;
    if(meta.isMethod(i))
    {
      fun = *bindI;
    }
    else if(bindI.active())
    {
      fun = *bindI.fun;
    }
    else if(bind.active())
    {
      fun = *bind.fun;
      fun = R_mkClosure(R_ClosureFormals(*fun), R_ClosureExpr(*fun), *enclI);
    }
    else
    {
      return;
    }
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
  if(!(REnv<>::is(env) && REnv<>::is(ns))) { return Rf_ScalarLogical(0); }
  if(!R_IsNamespaceEnv(ns))                { return Rf_ScalarLogical(0); }
  OoprLoad obj(env, ns);
  obj.loadEnv();
  return Rf_ScalarLogical(1);
}
catchR
