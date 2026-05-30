// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "meta.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
OoprMeta::OoprMeta(SEXP meta)
{
  if(!Rf_inherits(meta, "oopr_meta"))
  {
    Rf_error("`meta` is not of class \"oopr_meta\"");
  }
  const std::vector<std::string> nms = {
    "names", "access", "method", "property", "static", "class", "inherit"
   ,"virtual"
  };
  SEXP data = Rf_install("data");
  for(const std::string& nm : nms)
  {
    SEXP x = R_getVar(Rf_install(nm.c_str()), meta, FALSE);
    x = R_getVar(data, x, FALSE);
    this->meta.emplace(nm, x);
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
R_xlen_t OoprMeta::size()
{
  return Rf_xlength(meta["names"]);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP OoprMeta::name(const int& i)
{
  return Rf_install(getStr("names", i));
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP OoprMeta::inherit(const int& i)
{
  const char* out = getStr("inherit", i);
  if(strlen(out)) { return Rf_install(out); } else { return R_NilValue; }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isMethod(const int& i)
{
  return getLgl("method", i);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isProperty(const int& i)
{
  return strlen(getStr("property", i)) > 0;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isStatic(const int& i)
{
  return getLgl("static", i);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isClass(const int& i)
{
  return getLgl("class", i);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isInherit(const int& i)
{
  return strlen(getStr("inherit", i)) > 0;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isVirtual(const int& i)
{
  return getLgl("virtual", i);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::isAccess(const int& i, const char* access)
{
  return strcmp(getStr("access", i), access) == 0;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
int OoprMeta::which(const std::string &name)
{
  for(R_xlen_t i = 0; i < size(); ++i)
  {
    if(name == getStr("names", i)) return i;
  }
  return -1;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool OoprMeta::getLgl(const std::string& x, const int& i)
{
  return LOGICAL_ELT(meta[x], i) == 1;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
const char* OoprMeta::getStr(const std::string& x, const int& i)
{
  return CHAR(STRING_ELT(meta[x], i));
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP OoprMeta::subName(const std::string& access, const bool& inverse)
{
  const int size = Rf_xlength(meta["names"]);
  std::vector<std::string> names;
  names.reserve(size);
  for(int i = 0; i < size; ++i)
  {
    bool match = (access == getStr("access", i));
    if(inverse)  match = !match;
    if(match)    names.push_back(getStr("names", i));
  }
  pSEXP out = Rf_allocVector(STRSXP, names.size());
  for(int i = 0; i < (int)names.size(); ++i)
  {
    SET_STRING_ELT(out, i, Rf_mkChar(names[i].c_str()));
  }
  return out;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
