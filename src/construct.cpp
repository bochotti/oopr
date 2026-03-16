// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "construct.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool isOoprC(SEXP gen)
{
  if(!Rf_isS4(gen)) return false;
  const char* cls = CHAR(STRING_ELT(Rf_getAttrib(gen, R_ClassSymbol), 0));
  if(strcmp(cls, "ooprC") != 0) return false;

  SEXP name = Rf_getAttrib(gen, Rf_install("name"));
  if(!(Rf_isString(name) && Rf_xlength(name) == 1)) return false;

  SEXP inhr = Rf_getAttrib(gen, Rf_install("inhr"));
  if(!Rf_isString(inhr)) return false;

  SEXP meta = Rf_getAttrib(gen, Rf_install("meta"));
  if(!Rf_isEnvironment(meta)) return false;

  SEXP encl = Rf_getAttrib(gen, Rf_install("encl"));
  if(!Rf_isEnvironment(encl)) return false;

  return true;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP oopr_make(SEXP gen)
{
  if(!isOoprC(gen)) Rf_error("ooprC not called correctly");

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
bool is_inherited(SEXP frame, std::string& name)
{
  if(!Rf_isPairList(frame)) return false;
  const R_xlen_t len = Rf_xlength(frame);
  if(len < 2) return false;

  // get the callers environment
  for(R_xlen_t i = 0; i < (len - 3); ++i, frame = CDR(frame)) { }
  frame = CAR(frame);
  if(!Rf_isEnvironment(frame)) return false;

  // if this is a base class, then it will be in the enclosure of derived class
  frame = ENCLOS(frame);
  pSEXP sym = Rf_install(name.c_str());
  if(!R_existsVarInFrame(frame, sym)) return false;
  SEXP base = Rf_findVar(sym, frame);
  if(!isOoprC(base)) return false;
  sym = Rf_getAttrib(base, Rf_install("name"));
  return strcmp(name.c_str(), CHAR(STRING_ELT(sym, 0))) == 0;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP oopr_tidy(SEXP gen, SEXP encl, SEXP frame)
{
  if(!isOoprC(gen)) Rf_error("ooprC not called correctly");
  if(!Rf_isEnvironment(encl)) Rf_error("`encl` must be an environment");

  SEXP sthis = Rf_install("this");
  SEXP othis = Rf_findVar(sthis, encl);
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
    else
    {
      R_removeVarFromFrame(nm, othis);
      symlink(inhr, sthis, othis, nm);
    }
  }

  // remove constructor
  SEXP sym = Rf_install("name");
  std::string name(CHAR(STRING_ELT(Rf_getAttrib(gen, sym), 0)));
  R_removeVarFromFrame(Rf_install(name.c_str()), othis);

  // register & remove destructor
  name.insert(0, 1, '~');
  sym = Rf_install(name.c_str());
  if(R_existsVarInFrame(othis, sym))
  {
    R_RegisterFinalizer(othis, Rf_findVar(sym, othis));
    R_removeVarFromFrame(sym, othis);
  }
  name.erase(0, 1);

  // create the interface
  sym = Rf_install(".this");
  pSEXP nms;
  if(is_inherited(frame, name))
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
  pSEXP intf = interface(othis, sthis, nms, cls);
  Rf_defineVar(sym, intf, encl);

  // lock
  SEXP inhr = Rf_getAttrib(gen, Rf_install("inhr"));
  int size = Rf_xlength(inhr);
  for(int i = 0; i < size; ++i)
  {
    sym = Rf_installChar(STRING_ELT(inhr, i));
    R_LockEnvironment(Rf_findVar(sym, encl), FALSE);
  }
  R_LockEnvironment(intf, FALSE);
  R_LockEnvironment(othis, FALSE);
  R_LockEnvironment(encl, TRUE);
  return intf;
}

