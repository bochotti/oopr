// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "meta.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::is(const RObj<SEXP, ALLSXP>& x)
{
  if(!(x.type() == ENVSXP && x.inhr("oopr_meta"))) return false;
  const REnv<SEXP>  env(x);
  static RSym data("data");
  R_xlen_t    i{-1};
  for(std::pair<const char*, SEXPTYPE> spec : specifiers)
  {
    const REnv<SEXP>::Bind bind(env[spec.first]);
    if(!bind.exists())             return false;

    const RObj<SEXP> mem = bind.get();
    if(!REnv<>::is(mem))           return false;

    const REnv<SEXP> env2(mem);
    const auto bind2 = env2[data];
    if(!bind2.exists())            return false;

    const RObj<SEXP> mem2 = bind2.get();
    if(mem2.type() != spec.second) return false;

    if(i == -1) { i = mem2.size(); } else if(i != mem2.size()) { return false; }
  }
  return true;
}

SEXP OoprMeta::get(const REnv<SEXP>& x,  const char* nm)
{
  const REnv<SEXP> y(x[nm].get());
  static RSym data("data");
  return y[data].get();
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
OoprMeta::OoprMeta(const SEXP x, const bool check)
  : meta_(check ? (is(x) ? x : (stop("Not an OoprMeta"), R_NilValue)) : x)
#define SET(X) X##_(get(meta_, #X))
  , SET(names)
  , SET(access)
  , SET(method)
  , SET(property)
  , SET(static)
  , SET(class)
  , SET(inherit)
  , SET(virtual)
#undef SET
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
  const RStr<SEXP> str(inherit_[i]);
  return str.size() ? str : RSym(" ");
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isMethod(const int i) const
{
  return method_[i];
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isProperty(const int i) const
{
  return property_[i].size();
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
  return inherit_[i].size();
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isVirtual(const int i) const
{
  return virtual_[i];
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isAccess(const int i, const char* access) const
{
  return access_[i] == access;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
int OoprMeta::which(const std::string &name) const
{
  for(R_xlen_t i = 0; i < size(); ++i)
  {
    if(names_[i] == name) return i;
  }
  return -1;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
RChr<PSEXP> OoprMeta::subName(const std::string& access, const bool inverse)
  const
{
  const R_xlen_t size{this->size()};
  std::vector<std::string> names;
  names.reserve(size);
  for(int i = 0; i < size; ++i)
  {
    bool match = (access_[i] == access);
    if(inverse)  match = !match;
    if(match)    names.push_back(names_[i].data());
  }
  return names;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
