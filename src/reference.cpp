// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "reference.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class MemberReferences::symbols
{
public:
  symbols()
  {
    std::vector<std::string> names = {"$", "[[", "<-", "<<-", "=", "(", "{"};
    for(const std::string& name : names)
    {
      syms.emplace(name, Rf_install(name.c_str()));
    }
  }
  bool isDollar(SEXP x)  { return isSym(x) && x == syms["$"];  }
  bool isBracket(SEXP x) { return isSym(x) && x == syms["[["]; }
  bool isParen(SEXP x)  { return isSym(x) && x == syms["("]; }
  bool isCurly(SEXP x)   { return isSym(x) && x == syms["{"]; }
  bool isAssign(SEXP x)
  {
    return isSym(x) && (x == syms["<-"] || x == syms["="] || x == syms["<-"]);
  }
private:
  std::map<std::string, SEXP> syms;
  inline bool isSym(SEXP x) { return TYPEOF(x) == SYMSXP; }
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
MemberReferences::MemberReferences(SEXP expr) : sym(new symbols())
{
  if(TYPEOF(expr) == CLOSXP)
  {
    expr = BODY(expr);
  }
  walk(expr);
}
MemberReferences::~MemberReferences() { delete sym; }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
void MemberReferences::walk(SEXP e)
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
  case EXPRSXP:
  {
    R_xlen_t n = XLENGTH(e);
    for(R_xlen_t i = 0; i < n; ++i)
    {
      paths.push_back((int)(i + 1));
      parents.push_back(e);
      walk(VECTOR_ELT(e, i));
      parents.pop_back();
      paths.pop_back();
    }
    break;
  }
  default:
    break;
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
inline bool MemberReferences::isMemberRef(SEXP e)
{
  // must be a call
  if(TYPEOF(e) != LANGSXP)      return false;

  SEXP oper = CAR(e);
  if(TYPEOF(oper) != SYMSXP)    return false;

  // lhs must be a symbol
  if(TYPEOF(CADR(e)) != SYMSXP) return false;

  // dollar can have symbol or char
  if(sym->isDollar(oper))       return true;

  // brackets can vary, but I do not want to consider symbols
  SEXP rhs  = CADDR(e);
  return sym->isBracket(oper) && TYPEOF(rhs) == STRSXP && Rf_xlength(rhs) == 1L;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
std::string MemberReferences::classify(SEXP e, Match& m)
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
    if(sym->isAssign(CAR(parent)))
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
         (sym->isParen(CAR(parent)) && paths[i] == 2)
      || (sym->isCurly(CAR(parent)) && paths[i] == Rf_length(parent))
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

SEXP MemberReferences::getSrcRef(const Match& m)
{
  SEXP srcref     = Rf_install("srcref");
  std::size_t len = m.at.size();
  for(std::size_t i = len; i > 0; --i)
  {
    SEXP src = getAttrib(parents[i - 1], srcref);
    if(src != R_NilValue) return VECTOR_ELT(src, (R_xlen_t)paths[i - 1] - 1);
  }
  return R_NilValue;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP MemberReferences::toList()
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
    const Match& m = matches[(size_t)i];

    pSEXP iv = Rf_allocVector(INTSXP, (R_xlen_t)m.at.size());
    for(R_len_t k = 0; k < (R_xlen_t)m.at.size(); ++k)
    {
      INTEGER(iv)[k] = m.at[(size_t)k];
    }
    SET_VECTOR_ELT(at, i, iv);

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
SEXP findMemberRefs(SEXP expr)
{
  int type = TYPEOF(expr);
  switch(type)
  {
  case CLOSXP:
  {
    expr = BODY(expr);
  }
  case LANGSXP:
  {
    MemberReferences out(expr);
    return out.toList();
  }
  }
  if(!(type == ENVSXP || type == VECSXP)) return R_NilValue;

  R_xlen_t len = Rf_xlength(expr);
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
      SET_VECTOR_ELT(out, i, findMemberRefs(x));
    }
    break;
  }
  case VECSXP:
    names = Rf_getAttrib(expr, R_NamesSymbol);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      x = VECTOR_ELT(expr, i);
      SET_VECTOR_ELT(out, i, findMemberRefs(x));
    }
  }
  Rf_setAttrib(out, R_NamesSymbol, names);
  return out;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP findSrcRef(SEXP at, SEXP expr)
{
  if(TYPEOF(at) != INTSXP) Rf_error("`at` must be an integer");
  switch(TYPEOF(expr))
  {
  case CLOSXP:  expr = BODY(expr);
  case LANGSXP: break;
  default:      Rf_error("`expr` must be a call object");
  }

  R_xlen_t len = Rf_xlength(at);
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
