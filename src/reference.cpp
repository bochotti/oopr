// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "reference.h"
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Easy way to collect and compare symbols.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class Symbols
{
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  Symbols(std::initializer_list<std::string> syms)
  {
    for(const std::string& sym : syms)
    {
      this->syms.emplace(sym, Rf_install(sym.c_str()));
    }
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  bool is(SEXP x)
  {
    if(TYPEOF(x) != SYMSXP) return false;
    for(const auto& [key, val] : syms) if(x == val) return true;
    return false;
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  bool is(SEXP x, const std::initializer_list<std::string>& keys)
  {
    if(TYPEOF(x) != SYMSXP) return false;
    for(const std::string& key : keys) if(x == get(key)) return true;
    return false;
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  bool is(SEXP x, const std::string& key)
  {
    if(TYPEOF(x) != SYMSXP) return false;
    return x == get(key);
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP get(const std::string& key)
  {
    if(syms.find(key) == syms.end()) Rf_error("`%s` not a key", key.c_str());
    return syms[key];
  }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  std::map<std::string, SEXP> syms;
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Locate paths of members (`$` & `[[`) within a function body.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class MemberReferences
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
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

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  MemberReferences(SEXP expr)
  {
    if(TYPEOF(expr) == CLOSXP)
    {
      expr = BODY(expr);
    }
    walk(expr);
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP toList()
  {
    R_xlen_t n = (R_xlen_t)matches.size();

    pSEXP at   = Rf_allocVector(VECSXP, n);
    pSEXP type = Rf_allocVector(STRSXP, n);
    pSEXP oper = Rf_allocVector(STRSXP, n);
    pSEXP encl = Rf_allocVector(STRSXP, n);
    pSEXP memb = Rf_allocVector(STRSXP, n);
    pSEXP expr = Rf_allocVector(VECSXP, n);
    pSEXP src  = Rf_allocVector(VECSXP, n);

    for(R_xlen_t i = 0; i < n; ++i)
    {
      const Match& m = matches[(std::size_t)i];

      pSEXP iv = Rf_allocVector(INTSXP, (R_xlen_t)m.at.size());
      for(R_xlen_t j = 0; j < (R_xlen_t)m.at.size(); ++j)
      {
        INTEGER(iv)[j] = m.at[(std::size_t)j];
      }
      SET_VECTOR_ELT(at,   i, iv);
      SET_STRING_ELT(type, i, Rf_mkChar(m.type.c_str()));
      SET_STRING_ELT(oper, i, Rf_asChar(m.oper));
      SET_STRING_ELT(encl, i, Rf_asChar(m.encl));
      SET_STRING_ELT(memb, i, Rf_asChar(m.memb));
      SET_VECTOR_ELT(expr, i, m.expr);
      SET_VECTOR_ELT(src,  i, m.src);
    }

    pSEXP out = Rf_allocVector(VECSXP, 7);
    SET_VECTOR_ELT(out, 0, at);
    SET_VECTOR_ELT(out, 1, type);
    SET_VECTOR_ELT(out, 2, oper);
    SET_VECTOR_ELT(out, 3, encl);
    SET_VECTOR_ELT(out, 4, memb);
    SET_VECTOR_ELT(out, 5, expr);
    SET_VECTOR_ELT(out, 6, src);

    pSEXP names = Rf_allocVector(STRSXP, 7);
    SET_STRING_ELT(names, 0, Rf_mkChar("at"));
    SET_STRING_ELT(names, 1, Rf_mkChar("type"));
    SET_STRING_ELT(names, 2, Rf_mkChar("oper"));
    SET_STRING_ELT(names, 3, Rf_mkChar("encl"));
    SET_STRING_ELT(names, 4, Rf_mkChar("memb"));
    SET_STRING_ELT(names, 5, Rf_mkChar("expr"));
    SET_STRING_ELT(names, 6, Rf_mkChar("src"));
    Rf_setAttrib(out, R_NamesSymbol, names);

    return out;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:

  Symbols sym{"$", "[[", "<-", "<<-", "=", "(", "{"};

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  std::vector<int>   paths;
  std::vector<SEXP>  parents;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Walk over the expression object, collecting any member references.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void walk(SEXP e)
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
      m.src  = getSrcRef(m);
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
    if(TYPEOF(e) != LANGSXP)      return false;

    SEXP oper = CAR(e);
    if(TYPEOF(oper) != SYMSXP)    return false;

    // lhs must be a symbol
    if(TYPEOF(CADR(e)) != SYMSXP) return false;

    // dollar can have symbol or char
    if(sym.is(oper, "$"))       return true;

    // brackets can vary, but I do not want to consider symbols
    SEXP rhs  = CADDR(e);
    return sym.is(oper, "[[") && TYPEOF(rhs) == STRSXP && Rf_xlength(rhs) == 1;
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
    while(i > 1 && TYPEOF(parent) == LANGSXP && paths[i - 1] == 2)
    {
      --i;
      parent = parents[i];
      if(sym.is(CAR(parent), {"<-", "=", "<<-"}))
      {
        m.at.resize(i);
        m.expr = parent;
        return "assign";
      }
    }

    // check for calls, calls can be done inside (), e.g.
    //   (a$b)()
    //   {a ; a$b}()
    parent = parents.back();
    if(paths.back() == 1)
    {
      m.at.pop_back();
      m.expr = parent;
      return "call";
    }
    i = len - 1;
    while(i > 0 && TYPEOF(parent) == LANGSXP)
    {
      if(!(
           (sym.is(CAR(parent), "(") && paths[i] == 2)
        || (sym.is(CAR(parent), "{") && paths[i] == Rf_xlength(parent))
      )) break;
      --i;
      parent = parents[i];
      if(paths[i] == 1)
      {
        m.at.resize(i);
        m.expr = parent;
        return "call";
      }
    }

    // otherwise, its an access
    return "access";
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * obtain the srcref of a match.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  SEXP getSrcRef(const Match& m)
  {
    SEXP srcref           = Rf_install("srcref");
    const std::size_t len = m.at.size();
    for(std::size_t i = len; i > 0; --i)
    {
      SEXP src = getAttrib(parents[i - 1], srcref);
      if(src != R_NilValue) return VECTOR_ELT(src, (R_xlen_t)paths[i - 1] - 1);
    }
    return R_NilValue;
  }
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Gives the ability to loop over lists/environments.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template<typename T, typename... Args>
SEXP recurseExpr(SEXP expr, Args... args)
{
  int type = TYPEOF(expr);
  switch(type)
  {
  case CLOSXP:
  case LANGSXP:
  {
    T out(expr, args...);
    return out.toList();
  }
  default:      break;
  }
  if(!(type == ENVSXP || type == VECSXP)) return R_NilValue;

  const R_xlen_t len = Rf_xlength(expr);
  pSEXP out = Rf_allocVector(VECSXP, len);
  SEXP names, x;
  switch(type)
  {
  case ENVSXP:
  {
    names = R_lsInternal3(expr, TRUE, FALSE);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      x = Rf_findVar(Rf_installChar(STRING_ELT(names, i)), expr);
      SET_VECTOR_ELT(out, i, recurseExpr<T>(x, args...));
    }
    break;
  }
  case VECSXP:
    names = Rf_getAttrib(expr, R_NamesSymbol);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      x = VECTOR_ELT(expr, i);
      SET_VECTOR_ELT(out, i, recurseExpr<T>(x, args...));
    }
    break;
  default:
    break;
  }
  Rf_setAttrib(out, R_NamesSymbol, names);
  return out;
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Access point to the above class.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
SEXP findMemberRefs(SEXP expr)
{
  return recurseExpr<MemberReferences>(expr);
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Find source reference from a path.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
SEXP findSrcRef(SEXP at, SEXP expr)
{
  if(TYPEOF(at) != INTSXP) Rf_error("`at` must be an integer");
  switch(TYPEOF(expr))
  {
  case CLOSXP:  expr = BODY(expr);
  case LANGSXP: break;
  default:      Rf_error("`expr` must be a call object");
  }

  const R_xlen_t len = Rf_xlength(at);
  std::vector<int> path(len);
  for(R_xlen_t i = 0; i < len; ++i)
  {
    path[i] = INTEGER_ELT(at, i);
  }

  // collect each expr within the path
  std::vector<SEXP> parents(len);
  for(R_xlen_t i = 0; i < len; ++i)
  {
    parents[i] = expr;
    for(int j = 1; j < path[i]; expr = CDR(expr), ++j)
    {
      if(expr == R_NilValue) Rf_error("`at` is out of bounds");
    }
    expr = CAR(expr);
  }

  // find the srcref, which is an attribute of the immediate parent
  SEXP srcref = Rf_install("srcref");
  for(std::size_t i = len; i > 0; --i)
  {
    SEXP src = getAttrib(parents[i - 1], srcref);
    if(src != R_NilValue) return VECTOR_ELT(src, (R_xlen_t)path[i - 1] - 1);
  }
  return R_NilValue;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Find variables being used and created within a functions body.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class ExprUsage
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  ExprUsage(SEXP x, SEXP env = R_NilValue)
  {
    if(TYPEOF(x) == CLOSXP)
    {
      collectArgs(x);
      env = CLOENV(x);
      x   = BODY(x);
    }
    if(TYPEOF(env) != ENVSXP) Rf_error("`env` must be an environment");
    walk(x, env);
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP toList()
  {
    const R_xlen_t len = (R_xlen_t)missings.size();
    pSEXP var = Rf_allocVector(STRSXP, len);
    pSEXP src = Rf_allocVector(VECSXP, len);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      Missing& m = missings[i];
      SET_STRING_ELT(var, i, Rf_asChar(m.var));
      SET_VECTOR_ELT(src, i, m.src);
    }
    pSEXP out = Rf_allocVector(VECSXP, 2);
    SET_VECTOR_ELT(out, 0, var);
    SET_VECTOR_ELT(out, 1, src);
    pSEXP nms = Rf_allocVector(STRSXP, 2);
    SET_STRING_ELT(nms, 0, Rf_mkChar("var"));
    SET_STRING_ELT(nms, 1, Rf_mkChar("src"));
    Rf_setAttrib(out, R_NamesSymbol, nms);
    return out;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  Symbols assign{"<-", "=", "<<-"};
  Symbols subset{"$", "[[", "["};
  Symbols loop{"for"};
  Symbols fun{"function"};
  Symbols pkg{"::", ":::"};

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  std::vector<int>  paths;
  std::vector<SEXP> parents;
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
    SEXP args = FORMALS(x);
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
  bool exists(SEXP e, SEXP env)
  {
    if(TYPEOF(e) != SYMSXP) return false;
    for(const SEXP x : locals) if(e == x) return true;
    while(env != R_EmptyEnv)
    {
      if(R_existsVarInFrame(env, e)) return true;
      env = ENCLOS(env);
    }
    return false;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Walks the expression.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void walk(SEXP e, SEXP env)
  {
    if(TYPEOF(e) == LANGSXP)
    {
      if(assign.is(CAR(e)) && TYPEOF(CADR(e)) == SYMSXP)
      {
        // LHS are now local
        locals.push_back(CADR(e));
        walk(CADDR(e), env);
        return;
      }
      else if(subset.is(CAR(e)))
      {
        walk(CADR(e), env);
        // do not consider RHS of a $ call
        if(!subset.is(CAR(e), "$")) walk(CADDR(e), env);
        return;
      }
      else if(loop.is(CAR(e)))
      {
        // for(i in ...) { ... }, i is now a local
        locals.push_back(CADR(e));
        walk(CADDDR(e), env);
        return;
      }
      else if(pkg.is(CAR(e)))
      {
        pSEXP expr = Rf_mkString(CHAR(PRINTNAME(CADR(e))));
        expr = Rf_lang2(Rf_install("asNamespace"), expr);
        int err;
        R_tryEval(expr, R_GlobalEnv, &err);
        if(err) walk(CADR(e), env);
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
      if(!exists(e, env))
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
        walk(CAR(node), env);
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
  SEXP getSrcRef()
  {
    SEXP srcref           = Rf_install("srcref");
    const std::size_t len = paths.size();
    for(std::size_t i = len; i > 0; --i)
    {
      SEXP src = getAttrib(parents[i - 1], srcref);
      if(src != R_NilValue) return VECTOR_ELT(src, (R_xlen_t)paths[i - 1] - 1);
    }
    return R_NilValue;
  }
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
};
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Access point to the above class.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
SEXP getMissingVars(SEXP expr, SEXP env)
{
  return recurseExpr<ExprUsage>(expr, env);
}
