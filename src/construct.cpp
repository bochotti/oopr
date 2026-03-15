// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "construct.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
void checkOoprConstructor(SEXP gen)
{
  const char* cls = CHAR(STRING_ELT(Rf_getAttrib(gen, R_ClassSymbol), 0));
  if(!(Rf_isS4(gen) && strcmp(cls, "ooprC") == 0))
  {
    Rf_error("`gen` is not of class \"ooprC\"");
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP construct_make(SEXP gen)
{
  checkOoprConstructor(gen);

  SEXP inhr = Rf_getAttrib(gen, Rf_install("inhr"));
  int len = Rf_xlength(inhr);
  // create a new enclosure
  SEXP iencl = Rf_getAttrib(gen, Rf_install("encl"));
  pSEXP encl = R_NewEnv(ENCLOS(iencl), 1, 2 + len);

  // assign the inherited constructors
  for(int i = 0; i < len; ++i)
  {
    SEXP nm = Rf_installChar(STRING_ELT(inhr, i));
    Rf_defineVar(nm, Rf_findVar(nm, iencl), encl);
  }

  OoprMeta meta(Rf_getAttrib(gen, Rf_install("meta")));

  // create the new `this`
  SEXP sthis = Rf_install("this");
  SEXP ithis = Rf_findVar(sthis, iencl);
  len = Rf_xlength(ithis);
  pSEXP othis = R_NewEnv(encl, 1, len);

  // protect funs if created
  std::vector<pSEXP> funs;
  funs.reserve(len);

  for(int i = 0; i < len; ++i)
  {
    SEXP nm = meta.name(i);
    // inherited members link to inherited class, the enclosure does not exist
    if(meta.isInherit(i))
    {
      symlink(othis, meta.inherit(i), othis, nm, false);
    }
    else if(meta.isMethod(i))
    {
      funs.emplace_back(Rf_duplicate(Rf_findVar(nm, ithis)));
      if(!meta.isStatic(i)) SET_CLOENV(funs.back(), encl);
      Rf_defineVar(nm, funs.back(), othis);
      R_LockBinding(nm, othis);
    }
    else if(meta.isProperty(i))
    {
      funs.emplace_back(Rf_duplicate(R_ActiveBindingFunction(nm, ithis)));
      if(!meta.isStatic(i)) SET_CLOENV(funs.back(), encl);
      R_MakeActiveBinding(nm, funs.back(), othis);
    }
    else if(meta.isStatic(i))
    {
      symlink(ithis, sthis, othis, nm);
    }
    else
    {
      Rf_defineVar(nm, Rf_findVar(nm, ithis), othis);
    }
  }
  Rf_defineVar(sthis, othis, encl);
  return encl;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool is_inherited(SEXP stack, std::string& name)
{
  const R_xlen_t len = Rf_xlength(stack);
  if(len <= 3) return false;
  for(R_xlen_t i = 0; i < (len - 2); ++i)
  {
    stack = CDR(stack);
  }
  stack = CAR(stack);
  if(!(TYPEOF(stack) == LANGSXP && Rf_xlength(stack) == 2)) return false;
  SEXP call = CAR(stack);
  SEXP args = CADR(stack);
  return    Rf_xlength(call)     == 3
         && CAR(call)            == Rf_install("::")
         && CADR(call)           == Rf_install("base")
         && CADDR(call)          == Rf_install("force")
         && TYPEOF(args)         == LANGSXP
         && CAR(args)            == Rf_install(name.erase(0, 1).c_str());
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP construct_clean(SEXP gen, SEXP encl, SEXP stack)
{
  checkOoprConstructor(gen);
  if(!Rf_isEnvironment(encl)) Rf_error("`encl` must be an environment");

  SEXP othis = Rf_findVar(Rf_install("this"), encl);
  OoprMeta meta(Rf_getAttrib(gen, Rf_install("meta")));

  // inherited methods/properties can now be replaced
  const R_xlen_t len = Rf_xlength(othis);
  for(R_xlen_t i = 0; i < len; ++i)
  {
    if(!meta.isInherit(i)) continue;
    SEXP nm   = meta.name(i);
    SEXP inhr = Rf_findVar(meta.inherit(i), encl);
    if(meta.isMethod(i))
    {
      R_unLockBinding(nm, othis);
      R_removeVarFromFrame(nm, othis);
      Rf_defineVar(nm, Rf_findVar(nm, inhr), othis);
      R_LockBinding(nm, othis);
    }
    else if(meta.isProperty(i))
    {
      R_removeVarFromFrame(nm, othis);
      R_MakeActiveBinding(nm, R_ActiveBindingFunction(nm, inhr), othis);
    }
  }

  // remove constructor
  std::string name(CHAR(STRING_ELT(Rf_getAttrib(gen, Rf_install("name")), 0)));
  R_removeVarFromFrame(Rf_install(name.c_str()), othis);

  // register & remove destructor
  name.insert(0, 1, '~');
  SEXP sym = Rf_install(name.c_str());
  if(R_existsVarInFrame(othis, sym))
  {
    R_RegisterFinalizer(othis, Rf_findVar(sym, othis));
    R_removeVarFromFrame(sym, othis);
  }

  // create the interface
  sym = Rf_install(".this");
  pSEXP nms;
  if(is_inherited(stack, name))
  {
    // expose protected members
    nms = meta.subName("private", true);
  }
  else
  {
    nms = meta.subName("public");
  }

  SEXP cls = Rf_getAttrib(
    Rf_findVar(sym, Rf_getAttrib(gen, Rf_install("encl")))
   ,R_ClassSymbol
  );
  pSEXP intf = interface(othis, Rf_install("this"), nms, cls);
  Rf_defineVar(sym, intf, encl);

  // lock
  SEXP inhr = Rf_getAttrib(gen, Rf_install("inhr"));
  int size = Rf_xlength(inhr);
  for(int i = 0; i < size; ++i)
  {
    SEXP nm = Rf_installChar(STRING_ELT(inhr, i));
    R_LockEnvironment(Rf_findVar(nm, encl), FALSE);
  }
  R_LockEnvironment(intf, FALSE);
  R_LockEnvironment(othis, FALSE);
  R_LockEnvironment(encl, TRUE);
  return intf;
}

