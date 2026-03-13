// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef REFERENCE_H
#define REFERENCE_H
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Locate paths of members within a function body
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
#include "utils.h"
#include <vector>
#include <map>
#include <string>
#include <sstream>
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class MemberReferences
{
public:
  // for recording matches
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
  MemberReferences(SEXP expr);
  ~MemberReferences();
  SEXP toList();

private:
  class symbols;
  symbols *sym;
  std::vector<int>   paths;
  std::vector<SEXP>  parents;

  // walk over the expression
  void walk(SEXP e);

  // test for x$ or x[[]]
  inline bool isMemberRef(SEXP e);

  // classify a reference as access, assign, call
  std::string classify(SEXP e, Match& m);

  // obtain the srcref of a match
  SEXP getSrcRef(const Match& m);

};
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
extern "C" SEXP findMemberRefs(SEXP expr);
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
extern "C" SEXP findSrcRef(SEXP at, SEXP expr);
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
