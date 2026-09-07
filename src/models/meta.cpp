// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "meta.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#define LIST(X)                                                \
  X(names_,    "names",    RChr)                               \
  X(access_,   "access",   RChr)                               \
  X(method_,   "method",   RLgl)                               \
  X(property_, "property", RChr)                               \
  X(static_,   "static",   RLgl)                               \
  X(class_,    "class",    RLgl)                               \
  X(inherit_,  "inherit",  RChr)                               \
  X(virtual_,  "virtual",  RLgl)

#define ARGS2(arg1, arg2, arg3) SYM_2(arg1, arg2)
#define LIST2(X)                                               \
  X(data)                                                      \
  LIST(ARGS2)
SYMBOLS(LIST2, sym)
#undef  LIST2
#undef  ARGS2
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::is(const RObj<SEXP, ALLSXP> x)
{
  if(!(x.type() == ENVSXP && x.inhr("oopr_meta"))) return false;
  const REnv<SEXP>  env(x);
  R_xlen_t          i{-1};

#define CHECK(SYM, NAME, TYPE)                                      \
  {                                                                 \
    const REnv<SEXP>::Bind bind(env[sym.SYM]);                      \
    if(!bind.exists())                            { return false; } \
    const RObj<SEXP> mem = bind.get0();                             \
    if(!REnv<>::is(*mem))                         { return false; } \
    const REnv<SEXP> env2(mem);                                     \
    const REnv<SEXP>::Bind bind2(env2[sym.data]);                   \
    if(!bind2.exists())                           { return false; } \
    const RObj<SEXP> mem2(bind2.get0());                            \
    if(!TYPE<SEXP>::is(*mem2))                    { return false; } \
    if(i == -1)                                                     \
    {                                                               \
      i = mem2.size();                                              \
    }                                                               \
    else if(i != mem2.size())                                       \
    {                                                               \
      return false;                                                 \
    }                                                               \
  }
  LIST(CHECK)
#undef CHECK
  return true;
}

RObj<SEXP> get(const REnv<SEXP>& x, const RSym& nm)
{
  const REnv<SEXP> y(x[nm].get0());
  return y[sym.data].get0();
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
OoprMeta::OoprMeta(const RObj<SEXP, ALLSXP> x, const bool check)
  : meta_(check ? (is(x) ? x : (stop("Not an OoprMeta"), R_NilValue)) : x)
#define SET(SYM, NAME, TYPE) ,SYM(TYPE<SEXP>(get(meta_, sym.SYM)))
    LIST(SET)
#undef  SET
{ }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
R_xlen_t OoprMeta::size() const
{
  return names_.size();
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
RSym OoprMeta::name(const int i) const
{
  return names_[i];
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
RSym OoprMeta::inherit(const int i) const
{
  return strlen(inherit_[i]) ? inherit_[i] : " ";
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isMethod(const int i) const
{
  return method_[i];
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isProperty(const int i) const
{
  return strlen(property_[i]);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isStatic(const int i) const
{
  return static_[i];
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isClass(const int i) const
{
  return class_[i];
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isInherit(const int i) const
{
  return strlen(inherit_[i]);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isVirtual(const int i) const
{
  return virtual_[i];
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isAccess(const int i, const char* access) const
{
  return !std::strcmp(access_[i], access);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
int OoprMeta::which(const std::string &name) const
{
  for(R_xlen_t i = 0; i < size(); ++i)
  {
    if(name == names_[i]) return i;
  }
  return -1;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
RChr<PSEXP> OoprMeta::subName(const char* access, const bool inverse) const
{
  const R_xlen_t size{this->size()};
  std::vector<const char*> names;
  names.reserve(size);
  for(R_xlen_t i = 0; i < size; ++i)
  {
    bool match = !std::strcmp(access_[i], access);
    if(inverse)  match = !match;
    if(match)    names.push_back(names_[i]);
  }
  return names;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
