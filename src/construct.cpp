// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "construct.h"
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * A class with methods to create an instance of an oopr class.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class OoprInstance
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  OoprInstance(SEXP gen, SEXP name, SEXP frames)
    : gen(gen)
    , name(name)
    , meta(Rf_getAttrib(gen, syms["meta"]))
    , inhr(Rf_getAttrib(gen, syms["inhr"]))
    , encl(Rf_getAttrib(gen, syms["encl"]))
  {
    const R_xlen_t len = Rf_xlength(frames);
    if(len < 2) return;

    // find out if this class is being initialized as a base class
    for(R_xlen_t i = 0; i < (len - 3); ++i, frames = CDR(frames)) { }
    calr = CAR(frames);
    if(!Rf_isEnvironment(calr)) return;
    calr   = ENCLOS(calr);
    isInhr = is_ooprC(Rf_findVarInFrame(calr, name), name);
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP     gen;    // ooprC
  SEXP     name;   // SYMSXP
  OoprMeta meta;
  SEXP     inhr;   // VECSXP
  SEXP     encl;   // ENVSXP
  SEXP     calr;   // ENVSXP
  bool     isInhr = false;
  ppSEXP   inst;
  ppSEXP   thiz;
  ppSEXP   intf;   // ENSURE THIS DESTRUCTS LAST!

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Creates environment that holds `this` and base classes. Base classes
   * are ooprCs themselves, they will be initialized inside the constructor
   * method.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void makeEnclosure()
  {
    const R_xlen_t len = Rf_xlength(inhr);
    inst = R_NewEnv(ENCLOS(encl), 1, 2 + len);
    // assign the inherited constructors
    for(int i = 0; i < len; ++i)
    {
      SEXP nm = Rf_installChar(STRING_ELT(inhr, i));
      Rf_defineVar(nm, Rf_findVarInFrame(encl, nm), inst);
    }
    Rf_defineVar(syms[".this"], R_NilValue, inst);
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Create `this` environment, which holds all members defined in the
   * ooprC@encl. Methods & Properties use the new instance as their enclosure.
   * Static functions do not have an amended enclosure, and fields refer back
   * to the ooprC@encl via symlink.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void makeThis()
  {
    const R_xlen_t len = meta.size();
    thiz = R_NewEnv(inst, 1, len);
    SEXP from = Rf_findVarInFrame(encl, syms["this"]);

    // protect functions when created
    std::vector<pSEXP> funs;
    funs.reserve(len);

    for(R_xlen_t i = 0; i < len; ++i)
    {
      SEXP nm = meta.name(i);
      // if virtual, look forward to the caller and take its method
      if(isInhr && meta.isVirtual(i))
      {
        SEXP from = Rf_findVarInFrame(calr, syms["this"]);
        // if not an active binding in the caller then the caller
        // has defined the method and has not inherited it.
        if(R_existsVarInFrame(from, nm) && !R_BindingIsActive(nm, from))
        {
          Rf_defineVar(nm, Rf_findVarInFrame(from, nm), thiz);
          continue;
        }
      }
      // inherited members use symlink as their instances not yet initialized
      if(meta.isInherit(i))
      {
        symlink(thiz, meta.inherit(i), thiz, nm, false);
      }
      else if(meta.isMethod(i))
      {
        funs.emplace_back(Rf_duplicate(Rf_findVarInFrame(from, nm)));
        if(!meta.isStatic(i)) SET_CLOENV(funs.back(), inst);
        Rf_defineVar(nm, funs.back(), thiz);
        R_LockBinding(nm, thiz);
      }
      else if(meta.isProperty(i))
      {
        funs.emplace_back(Rf_duplicate(R_ActiveBindingFunction(nm, from)));
        if(!meta.isStatic(i)) SET_CLOENV(funs.back(), inst);
        R_MakeActiveBinding(nm, funs.back(), thiz);
      }
      else if(meta.isStatic(i))
      {
        symlink(from, syms["this"], thiz, nm);
      }
      else
      {
        Rf_defineVar(nm, Rf_findVarInFrame(from, nm), thiz);
      }
    }
    Rf_defineVar(syms["this"], thiz, inst);
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Moves the constructor function from `this` into the return value.
   * The constructor method is run from R (see construct.R) for easier
   * debugging, prevent long-jumps on errors, and allow catching condition
   * signals. The cpp class instance is attached as an external pointer for
   * further operations after the constructor method is run.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  SEXP moveConstructorWithXPtr()
  {
    pSEXP xptr = R_MakeExternalPtr(this, syms["OoprInstance"], inst);
    R_RegisterCFinalizerEx(xptr, (R_CFinalizer_t)OoprInstance::finalizer, TRUE);
    pSEXP out = Rf_findVarInFrame(thiz, name);
    R_removeVarFromFrame(name, thiz); // allows xptr to be finalized
    Rf_setAttrib(out, syms["xptr"], xptr);
    return out;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * In the makeThis method, inherited members were added to `this` via
   * symlink. Now that the base classes are initialized, their members
   * to be inherited can be added.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void replaceInheritedMembers()
  {
    const R_xlen_t len = meta.size();
    for(R_xlen_t i = 0; i < len; ++i)
    {
      if(!meta.isInherit(i) || (isInhr && meta.isVirtual(i))) continue;
      SEXP nm   = meta.name(i);
      SEXP inhr = Rf_findVarInFrame(inst, meta.inherit(i));
      if(meta.isMethod(i))
      {
        R_unLockBinding(nm, thiz);
        R_removeVarFromFrame(nm, thiz);
        Rf_defineVar(nm, Rf_findVarInFrame(inhr, nm), thiz);
        R_LockBinding(nm, thiz);
      }
      else if(meta.isProperty(i))
      {
        R_removeVarFromFrame(nm, thiz);
        R_MakeActiveBinding(nm, R_ActiveBindingFunction(nm, inhr), thiz);
      }
      else
      {
        R_removeVarFromFrame(nm, thiz);
        symlink(inhr, syms["this"], thiz, nm);
      }
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * If a destructor is defined for this class, register it.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void registerDestructor()
  {
    std::string name(Rf_translateChar(PRINTNAME(this->name)));
    name.insert(0, 1, '~');
    SEXP sym = Rf_install(name.c_str());

    if(R_existsVarInFrame(thiz, sym))
    {
      R_RegisterFinalizer(thiz, Rf_findVarInFrame(thiz, sym));
      R_removeVarFromFrame(sym, thiz);
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Creates the user-facing interface. If the class is being initialized
   * as a base class, expose the protected members.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void makeInterface()
  {
    pSEXP names;
    if(isInhr)
    {
      names = meta.subName("private", true);
    }
    else
    {
      names = meta.subName("public");
    }

    SEXP sym = syms[".this"];
    SEXP clazz = Rf_getAttrib(Rf_findVarInFrame(encl, sym), R_ClassSymbol);
    intf = interface(thiz, syms["this"], names, clazz);

    // interface can have the actual implementation if override via virtual
    if(isInhr)
    {
      SEXP thiz = Rf_findVarInFrame(encl, syms["this"]);
      const R_xlen_t len = meta.size();
      for(R_xlen_t i = 0; i < len; ++i)
      {
        if(!meta.isVirtual(i)) continue;
        SEXP  nm = meta.name(i);
        pSEXP fun;
        if(meta.isInherit(i))
        {
          SEXP inhr = Rf_findVarInFrame(inst, meta.inherit(i));
          fun = Rf_findVarInFrame(inhr, nm);
        }
        else
        {
          fun = Rf_duplicate(Rf_findVarInFrame(thiz, nm));
          SET_CLOENV(fun, inst);
        }
        R_unLockBinding(nm, intf);
        Rf_defineVar(nm, fun, intf);
        R_LockBinding(nm, intf);
      }
    }

    Rf_defineVar(sym, intf, inst);
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Locks bindings and paragraphs.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void lock()
  {
    const R_xlen_t len = Rf_xlength(inhr);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      SEXP sym = Rf_installChar(STRING_ELT(inhr, i));
      R_LockEnvironment(Rf_findVarInFrame(inst, sym), FALSE);
    }
    R_LockEnvironment(intf, FALSE);
    R_LockEnvironment(thiz, FALSE);
    R_LockEnvironment(inst, TRUE);
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  static inline Symbols syms{
    "name", "meta", "inhr", "encl", "this", ".this", "OoprInstance", "xptr"
  };

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Delete pointer
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  static void finalizer(SEXP xptr)
  {
    void* addr = R_ExternalPtrAddr(xptr);
    if(!addr) return;
    R_ClearExternalPtr(xptr);
    OoprInstance* obj = static_cast<OoprInstance*>(addr);
    delete obj;
  }
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP oopr_make(SEXP gen, SEXP name, SEXP frames)
{
  if(!(is_ooprC(gen, name) && Rf_isPairList(frames)))
  {
    Rf_error("ooprC not called correctly");
  }
  OoprInstance* obj = new OoprInstance(gen, name, frames);
  obj->makeEnclosure();
  obj->makeThis();
  return obj->moveConstructorWithXPtr();
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP oopr_tidy(SEXP gen)
{
  SEXP xptr = Rf_getAttrib(gen, OoprInstance::syms["xptr"]);
  if(!(   Rf_isFunction(gen)
       && TYPEOF(xptr) == EXTPTRSXP
       && R_ExternalPtrTag(xptr) == OoprInstance::syms["OoprInstance"]
  ))
  {
    Rf_error("ooprC not called correctly");
  }
  void* addr = R_ExternalPtrAddr(xptr);
  OoprInstance* obj = static_cast<OoprInstance*>(addr);
  obj->replaceInheritedMembers();
  obj->registerDestructor();
  obj->makeInterface();
  obj->lock();
  SEXP out = obj->intf;
  OoprInstance::finalizer(xptr);
  return out;
}
