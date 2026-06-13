// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "install.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class OoprLoad
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  OoprLoad(SEXP env, SEXP ns) : env(env), ns(ns)
  {
    if(!(Rf_isEnvironment(env) && R_IsNamespaceEnv(ns)))
    {
      status  = 1;
      message = "`env` is not an environment or `ns` not a namespace";
    }
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  int         status  = 0;
  std::string message = "";
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadEnv()
  {
    SEXP names = R_lsInternal3(env, TRUE, FALSE);
    const R_xlen_t len = Rf_xlength(names);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      SEXP name = Rf_installChar(STRING_ELT(names, i));
      SEXP oopr = R_getVar(name, env, FALSE);
      if(!is_ooprC(oopr, name)) continue;
      loadOopr(oopr);
      Rf_defineVar(name, oopr, ns);
    }
  }
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP env;
  SEXP ns;
  static inline Symbols sym{"encl", "meta", "inhr", "name", "this", ".this"};

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadOopr(SEXP ooprC)
  {
    SEXP inhr = Rf_getAttrib(ooprC, sym["inhr"]);
    SEXP encl = Rf_getAttrib(ooprC, sym["encl"]);
    OoprMeta meta(Rf_getAttrib(ooprC, sym["meta"]));

    const R_xlen_t len = Rf_xlength(inhr);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      SEXP name  = Rf_installChar(STRING_ELT(inhr, i));
      SEXP ooprI = R_getVar(name, encl, FALSE);
      loadInhr(name, ooprI, encl, meta);
    }

    SEXP thiz = R_getVar(sym["this"], encl, FALSE);
    for(R_xlen_t i = 0; i < meta.size(); ++i)
    {
      if(!meta.isClass(i) || meta.isInherit(i)) continue;
      SEXP name  = meta.name(i);
      SEXP ooprM = R_getVar(name, thiz, FALSE);
      loadClass(i, name, ooprM, thiz, meta);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadInhr(SEXP name, SEXP ooprI, SEXP encl, OoprMeta meta)
  {
    if(!fromAnotherPackage(ooprI)) return;
    SEXP enclI = Rf_getAttrib(ooprI, sym["encl"]);
    setLockedBinding(name, encl, ooprI);
    for(R_xlen_t i = 0; i < meta.size(); ++i)
    {
      if(meta.inherit(i) != name) continue;
      loadMember(i, meta, encl, enclI);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadClass(R_xlen_t& i, SEXP name, SEXP ooprM, SEXP thiz, OoprMeta meta)
  {
    SEXP memb = R_NilValue;
    if(meta.isStatic(i))
    {
      memb = ooprM;
      if(!Rf_inherits(memb, "oopr")) return;
      SEXP cls = STRING_ELT(Rf_getAttrib(ooprM, R_ClassSymbol), 0);
      cls = Rf_installChar(cls);
      ooprM = R_getVarEx(cls, Rf_topenv(R_EmptyEnv, ooprM), FALSE, R_NilValue);
    }
    if(!fromAnotherPackage(ooprM)) return;
    if(!meta.isStatic(i))
    {
      setLockedBinding(name, thiz, ooprM);
      return;
    }
    OoprMeta metaM(Rf_getAttrib(ooprM, sym["meta"]));
    for(R_xlen_t i = 0; i < metaM.size(); ++i)
    {
      if(!metaM.isStatic(i)) continue;
      SEXP encl  = R_ParentEnv(memb);
      SEXP enclM = Rf_getAttrib(ooprM, sym["encl"]);
      loadMember(i, metaM, encl, enclM);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  bool fromAnotherPackage(SEXP& ooprC)
  {
    if(!is_ooprC(ooprC)) return false;
    SEXP encl = Rf_getAttrib(ooprC, sym["encl"]);
    SEXP name = Rf_getAttrib(ooprC, sym["name"]);
    name = Rf_installChar(STRING_ELT(name, 0));
    SEXP top = Rf_topenv(R_EmptyEnv, encl);
    if(!R_IsNamespaceEnv(top))  return false;
    if(top == ns)               return false;
    SEXP ooprC2 = R_getVar(name, top, FALSE);
    if(!is_ooprC(ooprC2, name)) return false;
    ooprC = ooprC2;
    return true;
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadMember(R_xlen_t& i, OoprMeta meta, SEXP encl, SEXP enclI)
  {
    SEXP name  = meta.name(i);
    SEXP thiz  = R_getVar(sym["this"], encl, FALSE);
    SEXP thizI = R_getVar(sym["this"], enclI, FALSE);
    pSEXP fun;
    if(!Rf_isEnvironment(thiz)) return;
    if(!Rf_isEnvironment(thizI)) return;
    if(meta.isMethod(i))
    {
      fun = R_getVar(name, thiz, FALSE);
    }
    else
    {
      fun = R_ActiveBindingFunction(name, thiz);
    }
    fun = R_mkClosure(R_ClosureFormals(fun), R_ClosureExpr(fun), enclI);
    setLockedBinding(name, thiz, fun);
    if(meta.isStatic(i) && meta.isAccess(i, "public"))
    {
      thiz = R_getVar(sym[".this"], encl, FALSE);
      if(!Rf_isEnvironment(thiz)) return;
      setLockedBinding(name, thiz, fun);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void setLockedBinding(SEXP sym, SEXP env, SEXP value)
  {
    bool lock = R_BindingIsLocked(sym, env);
    bool bind = R_BindingIsActive(sym, env);
    if(lock) R_unLockBinding(sym, env);
    if(bind && Rf_isFunction(value))
    {
      R_MakeActiveBinding(sym, value, env);
    }
    else
    {
      Rf_defineVar(sym, value, env);
    }
    if(lock) R_LockBinding(sym, env);
  }
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP on_load(SEXP env, SEXP ns)
{
  OoprLoad obj(env, ns);
  if(obj.status) return Rf_ScalarLogical(0);
  obj.loadEnv();
  return Rf_ScalarLogical(1);
}
