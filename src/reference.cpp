// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "reference.h"
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Template for AST walking classes.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class ExprWalker
{
public:
  virtual ~ExprWalker() = default;
  virtual SEXP toList() = 0;
protected:
  std::vector<int>  paths;
  std::vector<SEXP> parents;
  virtual void walk(SEXP e) = 0;
  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * obtain the srcref of a match.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  SEXP getSrcRef()
  {
    SEXP srcref           = Rf_install("srcref");
    const std::size_t len = paths.size();
    for(std::size_t i = len; i > 0; --i)
    {
      SEXP src = Rf_getAttrib(parents[i - 1], srcref);
      const R_xlen_t j = (R_xlen_t)paths[i - 1] - 1;
      if(src != R_NilValue && j < Rf_xlength(src)) return VECTOR_ELT(src, j);
    }
    return R_NilValue;
  }
};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Gives the ability to loop over lists/environments.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template<typename T, typename... Args>
SEXP recurseExpr(SEXP expr, Args... args)
{
  static_assert(
    std::is_base_of<ExprWalker, T>::value, "T must derive ExprWalker"
  );
  int type = TYPEOF(expr);
  switch(type)
  {
  case CLOSXP:
  case LANGSXP:
  {
    T out(expr, args...);
    return out.toList();
  }
  case ENVSXP:
  case VECSXP: break;
  default:     return R_NilValue;
  }

  const R_xlen_t len = Rf_xlength(expr);
  RList<PSEXP> out(len);
  RChr<SEXP>   names;
  switch(type)
  {
  case ENVSXP:
  {
    REnv<SEXP> obj(expr);
    names = obj.names();
    for(R_xlen_t i = 0; i < len; ++i)
    {
      const RStr<SEXP> name(names[i]);
      out[i] = recurseExpr<T>(*obj[name], args...);
    }
    break;
  }
  case VECSXP:
  {
    RList<SEXP> obj(expr);
    names = obj.names();
    for(R_xlen_t i = 0; i < len; ++i)
    {
      const RStr<SEXP> name(names[i]);
      out[i] = recurseExpr<T>(*obj[name], args...);
    }
    break;
  }
  default:
    break;
  }
  out.attr(R_NamesSymbol) = names;
  return *out;
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Locate paths of members (`$` & `[[`) within a function body.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class MemberReferences : public ExprWalker
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  MemberReferences(SEXP expr)
  {
    if(TYPEOF(expr) == CLOSXP)
    {
      expr = R_ClosureExpr(expr);
    }
    walk(expr);
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP toList() override
  {
    const R_xlen_t n = (R_xlen_t)matches.size();

    RList<PSEXP> at(n);
    RChr <PSEXP> type(n);
    RChr <PSEXP> oper(n);
    RChr <PSEXP> encl(n);
    RChr <PSEXP> memb(n);
    RList<PSEXP> expr(n);
    RList<PSEXP> src(n);
    for(R_xlen_t i = 0; i < n; ++i)
    {
      const Match& m = matches[static_cast<std::size_t>(i)];
      at[i]   = RInt<SEXP>(m.at);
      type[i] = m.type.c_str();
      oper[i] = Rf_asChar(m.oper);
      encl[i] = Rf_asChar(m.encl);
      memb[i] = Rf_asChar(m.memb);
      expr[i] = m.expr;
      src[i]  = m.src;
    }
    RList<PSEXP> out{
      {"at",   *at}
     ,{"type", *type}
     ,{"oper", *oper}
     ,{"encl", *encl}
     ,{"memb", *memb}
     ,{"expr", *expr}
     ,{"src",  *src}
    };
    return *out;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  static const Symbols sym;
  struct Match
  {
    std::vector<int> at;
    std::string      type;
    SEXP             oper;
    SEXP             encl;
    SEXP             memb;
    SEXP             expr;
    SEXP             src;
  };
  std::vector<Match> matches;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Walk over the expression object, collecting any member references.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void walk(SEXP e) override
  {
    if(isMemberRef(e))
    {
      Match m;
      m.at   = paths;
      m.oper = CAR(e);
      m.encl = CADR(e);
      m.memb = CADDR(e);
      m.expr = e;
      m.type = classify(e, m);
      m.src  = getSrcRef();
      matches.emplace_back(std::move(m));
    }

    switch(TYPEOF(e))
    {
    case LANGSXP:
    case LISTSXP:
    {
      int i = 1;
      for(SEXP node = e; node != R_NilValue; node = CDR(node), ++i)
      {
        paths.push_back(i);
        parents.push_back(e);
        walk(CAR(node));
        parents.pop_back();
        paths.pop_back();
      }
      break;
    }
    default:
      break;
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * test for x$ or x[[]].
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  inline bool isMemberRef(SEXP e)
  {
    // must be a call
    if(!Rf_isLanguage(e))     return false;

    SEXP oper = CAR(e);
    if(!Rf_isSymbol(oper))    return false;

    // lhs must be a symbol
    if(!Rf_isSymbol(CADR(e))) return false;

    // dollar can have symbol or char
    if(sym.is(oper, "$"))     return true;

    // brackets can vary, but I do not want to consider symbols
    SEXP rhs  = CADDR(e);
    return sym.is(oper, "[[") && Rf_isString(rhs) && Rf_xlength(rhs) == 1;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * classify a reference as access, assign, call.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  std::string classify(SEXP e, Match& m)
  {
    const int len = paths.size();
    if(len == 0) return "access";
    SEXP parent = parents.back();
    int i = len;

    // Look for an assignment. As long as it is the second element, its likely
    // to be an assignment e.g.:
    //   a <- a$b <- 2L
    //   names(a$b) <- "name"
    while(i > 1 && Rf_isLanguage(parent) && paths[i - 1] == 2)
    {
      --i;
      parent = parents[i];
      if(sym.is(CAR(parent), {"<-", "=", "<<-"}))
      {
        m.expr = parent;
        return "assign";
      }
    }

    // check for calls, calls can be done inside (), e.g.
    //   (a$b)()
    //   {a ; a$b}()
    parent = parents.back();
    i = len - 1;
    if(isCall(i, parent))
    {
      m.expr = parent;
      return "call";
    }

    // otherwise, its an access
    i = len;
    parent = parents.back();
    while(i > 0 && Rf_isLanguage(parent) && sym.is(CAR(parent), {"$", "[[", "["}))
    {
      --i;
      parent = parents[i];
    }
    if(i < (len - 1))
    {
      if(isCall(i, parent))
      {
        m.expr = parent;
      }
      else
      {
        m.expr = parents[i + 1];
      }
    }
    return "access";
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  bool isCall(int i, SEXP& parent)
  {
    if(paths[i] == 1) return true;
    while(i > 0 && Rf_isLanguage(parent))
    {
      if(!(
           (sym.is(CAR(parent), "(") && paths[i] == 2)
        || (sym.is(CAR(parent), "{") && paths[i] == Rf_xlength(parent))
      )) break;
      --i;
      parent = parents[i];
      if(paths[i] == 1) return true;
    }
    return false;
  }
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // MemberReferences
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
const Symbols MemberReferences::sym{"$", "[[", "<-", "<<-", "=", "(", "{", "["};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Access point to the above class.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
SEXP find_member_refs(SEXP expr) try
{
  return recurseExpr<MemberReferences>(expr);
}
catchR

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Find variables being used and created within a functions body.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class ExprUsage : public ExprWalker
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  ExprUsage(SEXP x, SEXP env = R_NilValue)
  {
    if(TYPEOF(x) == CLOSXP)
    {
      collectArgs(x);
      env = R_ClosureEnv(x);
      x   = R_ClosureExpr(x);
    }
    if(!Rf_isEnvironment(env)) stop("`env` must be an environment");
    env_ = env;
    walk(x);
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP toList() override
  {
    const R_xlen_t len = missings.size();
    RChr <PSEXP> var(len);
    RList<PSEXP> src(len);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      const Missing& m = missings[i];
      var[i] = Rf_asChar(m.var);
      src[i] = m.src;
    }
    RList<PSEXP> out{ {"var", *var}, {"src", *src} };
    return *out;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  static const Symbols assign;
  static const Symbols subset;
  static const Symbols loop;
  static const Symbols fun;
  static const Symbols pkg;
  static const Symbols quo;
  SEXP env_;
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  std::vector<SEXP> locals;
  struct Missing
  {
    SEXP src;
    SEXP var;
  };
  std::vector<Missing> missings;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Grabs the names of the formals of a function
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void collectArgs(SEXP x)
  {
    SEXP args = R_ClosureFormals(x);
    while(args != R_NilValue)
    {
      locals.push_back(TAG(args));
      args = CDR(args);
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Checks if a symbol is within locals, otherwise it goes through the
   * search path of env
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool exists(SEXP e)
  {
    if(!Rf_isSymbol(e)) return false;
    for(const SEXP x : locals) if(e == x) return true;
    for(SEXP env = env_; env != R_EmptyEnv; env = R_ParentEnv(env))
    {
      if(R_existsVarInFrame(env, e)) return true;
    }
    return false;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Walks the expression.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void walk(SEXP e) override
  {
    if(Rf_isLanguage(e))
    {
      if(assign.is(CAR(e)) && Rf_isSymbol(CADR(e)))
      {
        // LHS are now local
        locals.push_back(CADR(e));
        walk(CADDR(e));
        return;
      }
      else if(subset.is(CAR(e)))
      {
        walk(CADR(e));
        // do not consider RHS of a $ call
        if(!subset.is(CAR(e), {"$", "@"})) walk(CADDR(e));
        return;
      }
      else if(loop.is(CAR(e)))
      {
        // for(i in ...) { ... }, i is now a local
        locals.push_back(CADR(e));
        walk(CADDDR(e));
        return;
      }
      else if(pkg.is(CAR(e)))
      {
        PSEXP expr = Rf_lang2(quo.get("quote"), CADR(e));
        expr = Rf_lang2(Rf_install("getNamespace"), expr);
        int err;
        R_tryEval(expr, R_GlobalEnv, &err);
        if(err) walk(CADR(e));
        return;
      }
      else if(isQuote(e))
      {
        return;
      }
      else if(fun.is(CAR(e)))
      {
        return;
      }
    }
    switch(TYPEOF(e))
    {
    case SYMSXP:
    {
      if(!exists(e))
      {
        Missing m;
        m.var = e;
        m.src = getSrcRef();
        missings.push_back(std::move(m));
      }
      break;
    }
    case LANGSXP:
    case LISTSXP:
    {
      int i = 1;
      for(SEXP node = e; node != R_NilValue; node = CDR(node), ++i)
      {
        paths.push_back(i);
        parents.push_back(e);
        walk(CAR(node));
        parents.pop_back();
        paths.pop_back();
      };
      break;
    }
    default:
      break;
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  bool isQuote(SEXP e)
  {
    if(!Rf_isLanguage(e)) return false;
    e = CAR(e);
    if(Rf_isLanguage(e) && pkg.is(CAR(e)) && CADR(e) == Rf_install("base"))
    {
      e = CADDR(e);
    }
    return quo.is(e);
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // ExprUsage
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
const Symbols ExprUsage::assign{"<-", "=", "<<-"};
const Symbols ExprUsage::subset{"$", "[[", "[", "@"};
const Symbols ExprUsage::loop{"for"};
const Symbols ExprUsage::fun{"function"};
const Symbols ExprUsage::pkg{"::", ":::"};
const Symbols ExprUsage::quo{"quote", "substitute", "bquote", "with", "within"};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Access point to the above class.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
SEXP get_missing_vars(SEXP expr, SEXP env) try
{
  return recurseExpr<ExprUsage>(expr, env);
}
catchR

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Find source reference from a path.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
SEXP find_src_ref(SEXP at, SEXP expr) try
{
  if(!Rf_isInteger(at)) stop("`at` must be an integer");
  switch(TYPEOF(expr))
  {
  case CLOSXP:  expr = R_ClosureExpr(expr);
  case LANGSXP: break;
  default:      stop("`expr` must be a call object");
  }

  const R_xlen_t len = Rf_xlength(at);
  const RInt<SEXP> path(at);

  // collect each expr within the path
  RList<PSEXP> parents(len);
  for(R_xlen_t i = 0; i < len; ++i)
  {
    parents[i] = expr;
    for(int j = 1; j < path[i]; expr = CDR(expr), ++j)
    {
      if(expr == R_NilValue) stop("`at` is out of bounds");
    }
    expr = CAR(expr);
  }

  // find the srcref, which is an attribute of the immediate parent
  SEXP srcref = Rf_install("srcref");
  for(R_xlen_t i = (len - 1); i >= 0; --i)
  {
    const RObj<SEXP, ALLSXP> parent = parents[i];
    const RObj<SEXP, ALLSXP> src    = parent.attr(srcref);
    if(*src == R_NilValue) continue;
    const R_xlen_t j = path[i] - 1;
    const RList<SEXP> src2 = src;
    if(j < src2.size()) return *src2[j];
  }
  return R_NilValue;
}
catchR
