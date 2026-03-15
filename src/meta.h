// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef META_H
#define META_H
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "utils.h"
#include <vector>
#include <string>
#include <map>
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Data model for the meta object
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class OoprMeta
{
public:
  OoprMeta(SEXP meta);
  SEXP name(const int& i);
  SEXP inherit(const int& i);
  bool isMethod(const int& i);
  bool isProperty(const int& i);
  bool isStatic(const int& i);
  bool isInherit(const int& i);
  SEXP subName(const std::string& access, const bool& inverse = false);
private:
  std::map<std::string, SEXP> meta;
  bool        getLgl(const std::string& x, const int& i);
  const char* getStr(const std::string& x, const int& i);
};
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
