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
    , meta(Rf_getAttrib(gen, sym["meta"]))
    , inhr(Rf_getAttrib(gen, sym["inhr"]))
    , encl(Rf_getAttrib(gen, sym["encl"]))
  {
    const R_xlen_t len = Rf_xlength(frames);
    if(len > 3)
    {
      // find out if this class is being initialized as a base class
      for(R_xlen_t i = 0; i < (len - 3); ++i, frames = CDR(frames)) { }
      calr = CAR(frames);
      if(Rf_isEnvironment(calr))
      {
        calr   = R_ParentEnv(calr);
        isInhr = is_ooprC(R_getVarEx(name, calr, FALSE, R_NilValue), name);
      }
    }
    while(CDR(frames) != R_NilValue) { frames = CDR(frames); }
    envr = CAR(frames);
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP     gen;    // ooprC
  SEXP     name;   // SYMSXP
  OoprMeta meta;
  SEXP     inhr;   // STRSXP
  SEXP     encl;   // ENVSXP
  SEXP     calr;   // ENVSXP
  SEXP     envr;   // ENVSXP
  bool     isInhr = false;
  pSEXP    inst;
  pSEXP    thiz;
  pSEXP    intf;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Creates environment that holds `this` and base classes. Base classes
   * are ooprCs themselves, they will be initialized inside the constructor
   * method.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void makeEnclosure()
  {
    const R_xlen_t len = Rf_xlength(inhr);
    inst = R_NewEnv(R_ParentEnv(encl), 1, 2 + len);
    // assign the inherited constructors
    for(int i = 0; i < len; ++i)
    {
      SEXP nm = Rf_installChar(STRING_ELT(inhr, i));
      Rf_defineVar(nm, R_getVar(nm, encl, FALSE), inst);
    }
    Rf_defineVar(sym[".this"], R_NilValue, inst);
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
    SEXP from = R_getVar(sym["this"], encl, FALSE);

    for(R_xlen_t i = 0; i < len; ++i)
    {
      SEXP nm = meta.name(i);
      // if virtual, look forward to the caller and take its method
      if(isInhr && meta.isVirtual(i))
      {
        SEXP from = R_getVar(sym["this"], calr, FALSE);
        // if not an active binding in the caller then the caller
        // has defined the method and has not inherited it.
        if(R_existsVarInFrame(from, nm) && !R_BindingIsActive(nm, from))
        {
          Rf_defineVar(nm, R_getVar(nm, from, FALSE), thiz);
          continue;
        }
      }
      // inherited members use symlink as their instances not yet initialized
      if(meta.isInherit(i))
      {
        symlinkR(thiz, meta.inherit(i), thiz, nm, false);
      }
      else if(meta.isMethod(i))
      {
        SEXP fun = R_getVar(nm, from, FALSE);
        Rf_defineVar(nm, dupeFun(fun, meta.isStatic(i)), thiz);
        R_LockBinding(nm, thiz);
      }
      else if(meta.isProperty(i))
      {
        SEXP fun = R_ActiveBindingFunction(nm, from);
        R_MakeActiveBinding(nm, dupeFun(fun, meta.isStatic(i)), thiz);
      }
      else if(meta.isStatic(i))
      {
        symlinkR(from, sym["this"], thiz, nm);
      }
      else
      {
        Rf_defineVar(nm, R_getVar(nm, from, FALSE), thiz);
      }
    }
    Rf_defineVar(sym["this"], thiz, inst);
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Evaluates and deletes the constructor method. The call is copied so
   * debugging / error looks better. Evaluation allows for unwinding.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void callConstructor()
  {
    SEXP fun  = R_getVar(name, thiz, FALSE);
    SEXP args = R_ClosureFormals(fun);

    pSEXP expr = Rf_allocVector(LANGSXP, Rf_length(args) + 1);
    SETCAR(expr, name);
    for(SEXP e = CDR(expr); e != R_NilValue; e = CDR(e), args = CDR(args))
    {
      SETCAR(e, TAG(args));
    }

    Rf_defineVar(name, fun, envr);
    R_removeVarFromFrame(name, thiz);
    RUnWind::eval(expr, envr);
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
      SEXP inhr = R_getVar(meta.inherit(i), inst, FALSE);
      if(meta.isMethod(i))
      {
        R_unLockBinding(nm, thiz);
        R_removeVarFromFrame(nm, thiz);
        Rf_defineVar(nm, R_getVar(nm, inhr, FALSE), thiz);
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
        symlinkR(inhr, sym["this"], thiz, nm);
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
      R_RegisterFinalizer(thiz, R_getVar(sym, thiz, FALSE));
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

    SEXP dthiz = sym[".this"];
    SEXP clazz = Rf_getAttrib(R_getVar(dthiz, encl, FALSE), R_ClassSymbol);
    intf = interface(thiz, sym["this"], names, clazz);

    // interface can have the actual implementation if override via virtual
    if(isInhr)
    {
      SEXP thiz = R_getVar(sym["this"], encl, FALSE);
      const R_xlen_t len = meta.size();
      for(R_xlen_t i = 0; i < len; ++i)
      {
        if(!meta.isVirtual(i)) continue;
        SEXP  nm = meta.name(i);
        pSEXP fun;
        if(meta.isInherit(i))
        {
          SEXP inhr = R_getVar(meta.inherit(i), inst, FALSE);
          fun = R_getVar(nm, inhr, FALSE);
        }
        else
        {
          fun = dupeFun(R_getVar(nm, thiz, FALSE), false);
        }
        R_unLockBinding(nm, intf);
        R_removeVarFromFrame(nm, intf);
        Rf_defineVar(nm, fun, intf);
        R_LockBinding(nm, intf);
      }
    }

    Rf_defineVar(dthiz, intf, inst);
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
      R_LockEnvironment(R_getVar(sym, inst, FALSE), FALSE);
    }
    R_LockEnvironment(intf, FALSE);
    R_LockEnvironment(thiz, FALSE);
    R_LockEnvironment(inst, TRUE);
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  static inline Symbols sym{
    "name", "meta", "inhr", "encl", "this", ".this", "base", "::", "sys.call"
  };
  SEXP dupeFun(SEXP fun, bool keep_env)
  {
    SEXP env = keep_env ? R_ClosureEnv(fun) : (SEXP)inst;
    pSEXP out = R_mkClosure(R_ClosureFormals(fun), R_ClosureExpr(fun), env);
    DUPLICATE_ATTRIB(out, fun);
    return out;
  }
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP oopr_make(SEXP gen, SEXP name, SEXP frames)
{
  if(!(is_ooprC(gen, name) && Rf_isPairList(frames)))
  {
    Rf_error("ooprC not called correctly");
  }
  try
  {
    OoprInstance obj = OoprInstance(gen, name, frames);
    obj.makeEnclosure();
    obj.makeThis();
    obj.callConstructor();
    obj.replaceInheritedMembers();
    obj.registerDestructor();
    obj.makeInterface();
    obj.lock();
    return obj.intf;
  }
  catch(const RUnWind::exception& e)
  {
  }
  return R_NilValue;
}
