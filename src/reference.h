// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef REFERENCE_H
#define REFERENCE_H
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Traverse an expression object to identify bits & bobs.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
#include "utils.h"
#include <vector>
#include <map>
#include <string>
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Locate paths of members (`$` & `[[`) within a function body.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
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
  class Symbols;
  Symbols *sym;
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
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Provide a function, expression, list/environment of, to get all
 * members references. Outputs:
 *   at:   the integer position to access via `[[`
 *   type: whether the operation is access, assign or call
 *   oper: `$` or `[[`
 *   encl: the name LHS of the operation
 *   memb: the name RHS of the operation
 *   expr: the expression
 *   src:  the source reference
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
extern "C" SEXP findMemberRefs(SEXP expr);
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Provide a path and function/expression to obtain the relevant source
 * reference, which identifies the position within the source file.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
extern "C" SEXP findSrcRef(SEXP at, SEXP expr);
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
