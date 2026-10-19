// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "construct.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#define LIST(X)                                                \
  X(thiz, "this")                                              \
  X(intf, ".this")                                             \
  X(rhs)
SYMBOLS(LIST, sym)
#undef  LIST
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Class members reside inside the body of an active binding function.
 * This allows for ensuring the "class" of the member is always the same.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
SEXP classmem_bind(RSym mem, Oopr obj, REnv<SEXP> env, RSym encl)
{
  const char* cls = obj.cls()[0].data();
  char text[1024];
  std::snprintf(text, sizeof(text), R"(
  {
    lhs <- obj;
    if(missing(rhs) || identical(rhs, lhs)) { return(lhs); }

    if(!is.oopr(rhs, "%s"))
    {
      stop(call. = FALSE,
        "Incoming value to member `%s` must be oopr class `%s`"
      );
    }
    if(match("%s", c("OoprVec", "OoprMap"), 0L) && lhs$class != rhs$class)
    {
      stop(call. = FALSE, sprintf(
        "Incoming %s to member `%s` must contain `%%s` classes"
       ,lhs$class
      ));
    }
    fun <- activeBindingFunction("%s", %s);
    oopr:::amend_plist(body(fun), 2:3, rhs);
    return(rhs);
  }
  )", cls, mem.c_str(), cls, cls, cls, mem.c_str(), mem.c_str(), encl.c_str());

  PSEXP body = R_ParseString(text);
  amend_plist(body, *RInt<PSEXP>{2, 3}, *obj);
  PSEXP arg = Rf_allocList(1);
  SET_TAG(arg, *sym.rhs);
  SETCAR(arg, R_MissingArg);
  return R_mkClosure(arg, *body, *env);
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * A class with methods to create an instance of an oopr class.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class OoprInstance
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  OoprInstance(SEXP gen, SEXP name, SEXP frames)
    : calr(getCalr(frames))
    , envr(CAR(Rf_lastElt(frames)))
    , name(name)
    , isInhr(*calr[this->name].get0() == gen)
    , ooprC(gen, !isInhr)
    , meta(ooprC.meta)
    , len(meta.size())
    , inst(ooprC.encl.parent(), true, 2 + ooprC.inhr.size())
    , thiz(inst, true, meta.size())
  { }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  const REnv<SEXP> calr;  // caller environment
  REnv<SEXP>       envr;  // ooprC@.Data() environment
  const RSym       name;
  bool             isInhr{false};
  const OoprC      ooprC;
  const OoprMeta&  meta;
  const R_xlen_t   len;
  REnv<PSEXP>      inst; // new instance enclosure
  REnv<PSEXP>      thiz; // new instance this
  REnv<PSEXP>      intf; // new instance .this

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Creates environment that holds `this` and base classes. Base classes
   * are ooprCs themselves, they will be initialized inside the constructor
   * method.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void makeEnclosure()
  {
    for(const RSym nm : ooprC.inhr) { inst[nm] = ooprC.encl[nm].get0(); }
    inst[sym.intf] = R_NilValue;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Create `this` environment, which holds all members defined in the
   * ooprC@encl. Methods & Properties use the new instance as their enclosure.
   * Static functions do not have an amended enclosure, and fields refer back
   * to the ooprC@encl via symlink.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void makeThis()
  {
    const REnv<SEXP> from(ooprC.oopr.thiz);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      const RSym nm = meta.name(i);
      if(nm == name)
      {
        if(!meta.isAccess(i, "public"))
        {
          if(meta.isAccess(i, "private"))
          {
            stop("%s constructor is private", nm.c_str());
          }
          if(!isInhr)
          {
            stop("%s constructor is protected", nm.c_str());
          }
        }
        if(!isInhr && meta.isAbstract(i))
        {
          stop("%s constructor is abstract", nm.c_str());
        }
      }
      const REnv<SEXP>::Bind fr(from[nm]);
      REnv<PSEXP>::Bind      to(thiz[nm]);
      // if virtual, look forward to the caller and take its method
      if(isInhr && meta.isVirtual(i))
      {
        const REnv<SEXP> from(calr[sym.thiz].get0());
        const REnv<SEXP>::Bind fr(from[nm]);
        // if not an active binding in the caller then the caller
        // has defined the method and has not inherited it.
        if(fr.exists() && !fr.active())
        {
          to = fr.get0();
          continue;
        }
      }
      // inherited members use symlink as their instances not yet initialized
      if(meta.isInherit(i))
      {
        symlinkR(*thiz, *meta.inherit(i), *thiz, *nm, false);
      }
      else if(meta.isMethod(i))
      {
        to = dupeFun(*fr.get0(), meta.isStatic(i));
        to.lock(true);
      }
      else if(meta.isProperty(i))
      {
        to.fun = dupeFun(*fr.fun, meta.isStatic(i));
      }
      else if(meta.isStatic(i))
      {
        symlinkR(*from, *sym.thiz, *thiz, *nm, false);
      }
      else
      {
        to = fr.get0();
      }
    }
    inst[sym.thiz] = thiz;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Evaluates and deletes the constructor method. The call is copied so
   * debugging / error looks better. Evaluation allows for unwinding.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void callConstructor()
  {
    REnv<PSEXP>::Bind bind = thiz[name];
    SEXP fun               = *bind.get0();
    SEXP body              = R_ClosureExpr(fun);
    const bool run         = (Rf_xlength(body) > 1);
    PSEXP expr;
    if(run)
    {
      SEXP args = R_ClosureFormals(fun);
      expr = Rf_allocVector(LANGSXP, Rf_length(args) + 1);
      SETCAR(expr, *name);
      for(SEXP e = CDR(expr); e != R_NilValue; e = CDR(e), args = CDR(args))
      {
        SETCAR(e, TAG(args));
      }
      envr[name] = fun;
    }
    bind.lock(false);
    bind.remove();
    if(run)
    {
      RUnWind::eval(expr, *envr);
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * In the makeThis method, inherited members were added to `this` via
   * symlink. Now that the base classes are initialized, their members
   * to be inherited can be added.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void replaceInheritedMembers()
  {
    for(R_xlen_t i = 0; i < len; ++i)
    {
      if(!meta.isInherit(i) || (isInhr && meta.isVirtual(i))) { continue; }
      const RSym         nm(meta.name(i));
      const REnv<SEXP>   inhr(inst[meta.inherit(i)]);

      const REnv<SEXP>::Bind fr(inhr[nm]);
      REnv<PSEXP>::Bind      to(thiz[nm]);

      if(meta.isMethod(i))
      {
        to.lock(false);
        to.remove();
        to = fr.get0();
        to.lock(true);
      }
      else if(meta.isProperty(i) || fr.active())
      {
        to.remove();
        to.fun = fr.fun;
      }
      else
      {
        to.remove();
        if(fr.active())
        {
          to.fun = fr.fun;
        }
        else
        {
          symlinkR(*inhr, *sym.thiz, *thiz, *nm, false);
        }
      }
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Replaces class members with an active binding which asserts type.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void encapsulateClassMembers()
  {
    for(R_xlen_t i = 0; i < len; ++i)
    {
      if(!meta.isClass(i) || meta.isInherit(i) || meta.isStatic(i))
      {
        continue;
      }
      const RSym        nm(meta.name(i));
      REnv<PSEXP>::Bind to(thiz[nm]);
      PSEXP obj = *thiz[nm];
      to.remove();
      to.fun    = classmem_bind(nm, Oopr(obj, false), inst, sym.thiz);
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * If a destructor is defined for this class, register it.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void registerDestructor()
  {
    std::string name{this->name.c_str()};
    name.insert(0, 1, '~');
    RSym nm = name.c_str();

    REnv<PSEXP>::Bind bind = thiz[nm];
    if(bind.exists())
    {
      R_RegisterFinalizer(*thiz, *bind.get0());
      bind.lock(false);
      bind.remove();
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Creates the user-facing interface. If the class is being initialized
   * as a base class, expose the protected members.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void makeInterface()
  {
    const RChr<PSEXP> names(
      meta.subName(isInhr ? "private" : "public", isInhr, name.c_str())
    );
    const RChr<SEXP> clazz(ooprC.oopr.cls());
    intf = interface(*thiz, *sym.thiz, *names, *clazz, false);

    // interface can have the actual implementation if override via virtual
    if(isInhr)
    {
      const REnv<SEXP> thiz(ooprC.oopr.thiz);
      const R_xlen_t len = meta.size();
      for(R_xlen_t i = 0; i < len; ++i)
      {
        if(!meta.isVirtual(i)) continue;
        const RSym nm = meta.name(i);
        const PSEXP fun(
          meta.isInherit(i) ? *REnv<SEXP>(inst[meta.inherit(i)])[nm].get0()
                            : dupeFun(*thiz[nm].get0(), false)
        );
        REnv<PSEXP>::Bind to(intf[nm]);
        to.lock(false);
        to.remove();
        to = fun;
        to.lock(true);
      }
    }
    inst[sym.intf] = intf;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Locks environments and enclosures bindings.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void lock()
  {
    intf.lock(false);
    thiz.lock(false);
    inst.lock(true);
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  REnv<SEXP> getCalr(SEXP frames) const
  {
    SEXP up = Rf_elt(frames, Rf_xlength(frames) - 3);
    return REnv<>::is(up) ? R_ParentEnv(up) : R_EmptyEnv;
  }
  SEXP dupeFun(SEXP fun, bool keep_env)
  {
    const REnv<SEXP> env(keep_env ? R_ClosureEnv(fun) : inst.sexp());
    PSEXP out = R_mkClosure(R_ClosureFormals(fun), R_ClosureExpr(fun), *env);
    DUPLICATE_ATTRIB(out, fun);
    return out;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // OoprInstance
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP oopr_make(SEXP gen, SEXP name, SEXP frames) try
{
  if(!(Rf_inherits(gen, "ooprC") && RSym::is(name) && Rf_isPairList(frames)))
  {
    stop("ooprC not called correctly");
  }
  OoprInstance obj(gen, name, frames);
  obj.makeEnclosure();
  obj.makeThis();
  obj.callConstructor();
  obj.replaceInheritedMembers();
  obj.encapsulateClassMembers();
  obj.registerDestructor();
  obj.makeInterface();
  obj.lock();
  return *obj.intf;
}
catchR

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP cmem_bindfun(SEXP mem, SEXP obj, SEXP env, SEXP sym) try
{
  struct make { static RSym sym(SEXP x, const char* nm)
  {
    if(RSym::is(x)) return x;
    if(RChr<>::is(x))
    {
      const RChr<SEXP> chr(x);
      if(chr.size() == 1) return chr[0].sym();
    }
    stop("`%s` must be a symbol or single character vector", nm);
    return R_NilValue;
  }};
  RSym mem_(make::sym(mem, "mem"));
  RSym sym_(make::sym(sym, "sym"));

  if(!Oopr::is(obj))   { stop("`obj` must be an oopr"); }
  if(!REnv<>::is(env)) { stop("`env` must be an environment"); }
  Oopr       obj_(obj, false);
  REnv<SEXP> env_(env);

  return classmem_bind(mem_, obj_, env_, sym_);
}
catchR
