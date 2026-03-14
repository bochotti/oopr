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
  int size = Rf_xlength(inhr);
  // create a new enclosure
  SEXP iencl = Rf_getAttrib(gen, Rf_install("encl"));
  pSEXP encl = R_NewEnv(ENCLOS(iencl), 1, 2 + size);

  // do something with inherited classes
  for(int i = 0; i < size; ++i)
  {

  }

  OoprMeta meta(Rf_getAttrib(gen, Rf_install("meta")));

  // create the new `this`
  SEXP ithis = Rf_findVar(Rf_install("this"), iencl);
  size = Rf_xlength(ithis);
  pSEXP othis = R_NewEnv(encl, 1, size);

  // protect funs if created
  std::vector<pSEXP> funs;
  funs.reserve(size);

  for(int i = 0; i < size; ++i)
  {
    SEXP nm = meta.name(i);
    if(meta.isMethod(i))
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
      symlink(ithis, Rf_install("this"), othis, nm);
    }
    else
    {
      Rf_defineVar(nm, Rf_findVar(nm, ithis), othis);
    }
  }
  Rf_defineVar(Rf_install("this"), othis, encl);
  return encl;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP construct_clean(SEXP gen, SEXP encl)
{
  checkOoprConstructor(gen);
  if(!Rf_isEnvironment(encl)) Rf_error("`encl` must be an environment");

  SEXP othis = Rf_findVar(Rf_install("this"), encl);

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
  OoprMeta meta(Rf_getAttrib(gen, Rf_install("meta")));
  pSEXP nms = meta.subName("public");
  SEXP cls = Rf_getAttrib(
    Rf_findVar(sym, Rf_getAttrib(gen, Rf_install("encl")))
   ,R_ClassSymbol
  );
  pSEXP intf = interface(othis, Rf_install("this"), nms, cls);
  Rf_defineVar(sym, intf, encl);

  R_LockEnvironment(othis, FALSE);
  R_LockEnvironment(encl, TRUE);
  return intf;
}
