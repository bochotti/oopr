// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "meta.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
ooprMeta::ooprMeta(SEXP meta)
{
  const char* cls = CHAR(STRING_ELT(Rf_getAttrib(meta, R_ClassSymbol), 0));
  if(strcmp(cls, "oopr_meta") != 0)
  {
    Rf_error("`meta` is not of class \"oopr_meta\"");
  }

  std::vector<std::string> nms = {
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
SEXP ooprMeta::name(int& i)
{
  return Rf_install(getStr("names", i));
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool ooprMeta::isMethod(int& i)
{
  return getLgl("method", i);
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool ooprMeta::isProperty(int& i)
{
  return strlen(getStr("property", i)) > 0;
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool ooprMeta::isStatic(int& i)
{
  return getLgl("static", i);
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
bool ooprMeta::getLgl(std::string x, int& i)
{
  return LOGICAL_ELT(meta[x], i) == 1;
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
const char* ooprMeta::getStr(std::string x, int& i)
{
  return CHAR(STRING_ELT(meta[x], i));
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
pSEXP ooprMeta::subName(std::string access, bool inverse)
{
  int size = Rf_xlength(meta["names"]);
  std::vector<std::string> names;
  names.reserve(size);
  for(int i = 0; i < size; ++i)
  {
    bool match = access == getStr("access", i);
    if(inverse) match = !match;
    if(match) names.push_back(getStr("names", i));
  }
  pSEXP out = Rf_allocVector(STRSXP, names.size());
  for(int i = 0; i < (int) names.size(); ++i)
  {
    SET_STRING_ELT(out, i, Rf_mkChar(names[i].c_str()));
  }
  return out;
}
