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
      SEXP oopr = Rf_findVarInFrame(env, name);
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
  Symbols syms{"encl", "meta", "inhr", "this", ".this"};

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadOopr(SEXP ooprC)
  {
    SEXP inhr = Rf_getAttrib(ooprC, syms.get("inhr"));
    SEXP encl = Rf_getAttrib(ooprC, syms.get("encl"));
    OoprMeta meta(Rf_getAttrib(ooprC, syms.get("meta")));
    const R_xlen_t len = Rf_xlength(inhr);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      SEXP name  = Rf_installChar(STRING_ELT(inhr, i));
      SEXP ooprI = Rf_findVarInFrame(encl, name);
      if(!is_ooprC(ooprI, name)) continue;
      loadInhr(name, ooprI, encl, meta);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadInhr(SEXP name, SEXP ooprI, SEXP encl, OoprMeta meta)
  {
    SEXP enclI = Rf_getAttrib(ooprI, syms.get("encl"));
    // SEXP top   = Rf_eval(Rf_lang2(Rf_install("topenv"), enclI), R_BaseEnv);
    SEXP top = Rf_topenv(R_EmptyEnv, enclI);
    if(!R_IsNamespaceEnv(top)) return;
    if(top == ns)              return;
    ooprI = Rf_findVarInFrame(top, name);
    if(!is_ooprC(ooprI, name)) return;
    enclI = Rf_getAttrib(ooprI, syms.get("encl"));
    setLockedBinding(name, encl, ooprI);
    for(R_xlen_t i = 0; i < meta.size(); ++i)
    {
      if(meta.inherit(i) != name) continue;
      loadMember(i, meta, encl, enclI);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void loadMember(R_xlen_t i, OoprMeta meta, SEXP encl, SEXP enclI)
  {
    SEXP name  = meta.name(i);
    SEXP thiz  = Rf_findVarInFrame(encl,  syms.get("this"));
    SEXP thizI = Rf_findVarInFrame(enclI, syms.get("this"));
    SEXP fun;
    if(!Rf_isEnvironment(thiz)) return;
    if(!Rf_isEnvironment(thizI)) return;
    if(meta.isMethod(i))
    {
      fun = Rf_findVarInFrame(thiz, name);
    }
    else
    {
      fun = R_ActiveBindingFunction(name, thiz);
    }
    SET_CLOENV(fun, enclI);
    if(meta.isStatic(i) && meta.isAccess(i, "public"))
    {
      thiz = Rf_findVarInFrame(encl, syms.get(".this"));
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
