// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "meta.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
OoprMeta::OoprMeta(SEXP meta)
{
  const char* cls = CHAR(STRING_ELT(Rf_getAttrib(meta, R_ClassSymbol), 0));
  if(strcmp(cls, "oopr_meta") != 0)
  {
    Rf_error("`meta` is not of class \"oopr_meta\"");
  }

  const std::vector<std::string> nms = {
    "names", "access", "method", "property", "static", "class", "inherit"
  };
  SEXP data = Rf_install("data");
  for(const std::string& nm : nms)
  {
    SEXP x = Rf_findVar(Rf_install(nm.c_str()), meta);
    x = Rf_findVar(data, x);
    this->meta.emplace(nm, x);
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP OoprMeta::name(const int& i)
{
  return Rf_install(getStr("names", i));
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
bool OoprMeta::getLgl(const std::string x, const int& i)
{
  return LOGICAL_ELT(meta[x], i) == 1;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
const char* OoprMeta::getStr(const std::string x, const int& i)
{
  return CHAR(STRING_ELT(meta[x], i));
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
pSEXP OoprMeta::subName(const std::string access, const bool inverse)
{
  int size = Rf_xlength(meta["names"]);
  std::vector<std::string> names;
  names.reserve(size);
  for(int i = 0; i < size; ++i)
  {
    bool match = (access == getStr("access", i));
    if(inverse)  match = !match;
    if(match)    names.push_back(getStr("names", i));
  }
  pSEXP out = Rf_allocVector(STRSXP, names.size());
  for(int i = 0; i < (int) names.size(); ++i)
  {
    SET_STRING_ELT(out, i, Rf_mkChar(names[i].c_str()));
  }
  return out;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
